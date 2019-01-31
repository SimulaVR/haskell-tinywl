{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TinyWL where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM

import           Foreign
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Text.XkbCommon.Keysym
import           Text.XkbCommon.KeyboardState

import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.WlRoots.Compositor
import           Graphics.Wayland.WlRoots.Output
import           Graphics.Wayland.WlRoots.Surface
import           Graphics.Wayland.WlRoots.Backend
import           Graphics.Wayland.Signal
import           Graphics.Wayland.WlRoots.Render
import           Graphics.Wayland.WlRoots.Render.Color
import           Graphics.Wayland.WlRoots.OutputLayout
import           Graphics.Wayland.WlRoots.Input
import           Graphics.Wayland.WlRoots.Seat
import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.XCursorManager
import           Graphics.Wayland.WlRoots.XdgShell
import           Graphics.Wayland.WlRoots.Input.Keyboard

import           System.Clock

-- Turn on inline-C for select calls that hsroots doesn't provide.
C.initializeTinyWLCtxAndIncludes

data TinyWLCursorMode = TinyWLCursorPassthrough | TinyWLCursorMove |  TinyWLCursorResize

data TinyWLServer = TinyWLServer { _tsBackend :: Ptr Backend
                                 , _tsRenderer :: Ptr Renderer
                                 , _tsXdgShell :: Ptr WlrXdgShell
                                 , _tsNewXdgSurface :: WlListener ()
                                 , _tsViews :: TVar [TinyWLView]
                                 , _tsCursor :: Ptr WlrCursor
                                 , _tsCursorManager :: Ptr WlrXCursorManager
                                 , _tsCursorMotion :: WlListener ()
                                 , _tsCursorMotionAbsolute :: WlListener ()
                                 , _tsCursorButton :: WlListener ()
                                 , _tsCursorAxis :: WlListener ()
                                 , _tsSeat :: Ptr WlrSeat
                                 , _tsNewInput :: WlListener ()
                                 , _tsRequestCursor :: WlListener ()
                                 , _tsKeyboards :: TVar [Ptr WlrKeyboard]
                                 , _tsCursorMode :: TinyWLCursorMode
                                 , _tsGrabbedView :: TinyWLView
                                 , _tsGrabX :: Double
                                 , _tsGrabY :: Double
                                 , _tsGrabWidth :: Int
                                 , _tsResizeEdges :: Int
                                 , _tsOutputLayout :: Ptr WlrOutputLayout
                                 , _tsOutputs :: TVar [TinyWLOutput]
                                 , _tsNewOutput :: WlListener ()
                                 }

data TinyWLOutput = TinyWLOutput { _toServer :: TinyWLServer
                                , _toWlrOutput :: Ptr WlrOutput
                                , _toFrame :: WlListener ()
                                }

data TinyWLView = TinyWLView { _tvServer :: TinyWLServer
                             , _tvXdgSurface :: Ptr WlrXdgSurface
                             , _tvMap :: WlListener ()
                             , _tvUnmap :: WlListener ()
                             , _tvDestroy :: WlListener ()
                             , _tvRequestMove :: WlListener ()
                             , _tvRequestResize :: WlListener ()
                             , _tvMapped :: Bool
                             , _tvX :: Int
                             , _tvY :: Int
                             }

data TinyWLKeyboard = TinyWLKeyboard { _tkServer :: TinyWLServer
                                     , _tkDevice :: Ptr InputDevice
                                     , _tkModifiers :: WlListener ()
                                     , _tkKey :: WlListener ()
                                     }

data RenderData = RenderData { _rdOutput :: Ptr WlrOutput
                             , _rdrRenderer :: Ptr Renderer
                             , _rdView :: TinyWLView
                             , _rdWhen :: TimeSpec
                             }

-- | We will say that two views are "the same" when they have the same Ptr WlrXdgSurface (this is
-- | is a bad idea long-term but for now will suffice; for example, we might have two
-- | distinct views [one used as an icon and one a window] that have the same surface).
-- | TODO: Give TinyWLView's a unique id and change this function accordingly.
isSameView :: TinyWLView -> TinyWLView -> Bool
isSameView tinyWLView1 tinyWLView2 = (_tvXdgSurface tinyWLView1) == (_tvXdgSurface tinyWLView2)

-- | A helper function for `TVar [TinyWLView]` mutation.
removeTinyWLViewFromList :: TinyWLView -> [TinyWLView] -> [TinyWLView]
removeTinyWLViewFromList tinyWLView list = filter (not . isSameView tinyWLView) list

-- This function takes a TinyWLView to (i) defocus the keyboard's old surface and (ii)
-- focus on the new TinyWLView. (To understand this function, mostly ignore the second
-- argument).

-- When a view is "defocused" a call to `wlr_xdg_toplevel_set_activated` is made
-- (w/false flag). When a view is "focused" it is (i) sent to the head of the
-- server's list of views (this involves mutation); (ii) called with
-- wlr_xdg_toplevel_set_activated (w/true flag); (iii) called with
-- wlr_seat_keyboard_notify_enter.

-- NOTE: It's unclear why this function doesn't just take a TinyWLView, and then
-- use that to extract a Ptr WlrSurface. For now I'll keep the second argument
-- (which seems redundant) to mirror the C implementation.
focusView :: TinyWLView -> Ptr WlrSurface -> IO ()
focusView tinyWLView ptrWlrSurface = do
  let ptrWlrSeat =_tsSeat (_tvServer tinyWLView)
  prevSurface <- getKeyboardFocus (getKeyboardState ptrWlrSeat)
  when (ptrWlrSurface /= prevSurface) $ do -- If we're already focused on this view/surface, then this function doesn't do anything.
          when (prevSurface /= nullPtr) $ deactivatePreviouslyFocusedSurface prevSurface
          mutateViewToFront tinyWLView
          setActivated (_tvXdgSurface tinyWLView) True
          keyboardNotifyEnterIntoNewSurface ptrWlrSeat (_tvXdgSurface tinyWLView)
  where
        -- |Let's client know it no longer has focus (so it can, i.e., stop displaying caret).
        deactivatePreviouslyFocusedSurface prevSurface = do
            let prevSurface' = (toInlineC prevSurface) :: Ptr C'WlrSurface
            prevSurfaceXdg <- [C.exp| struct wlr_xdg_surface * { wlr_xdg_surface_from_wlr_surface( $(struct wlr_surface * prevSurface') ) }|] -- hsroots lacks this function so we use inline-C
            setActivated (toC2HS prevSurfaceXdg) False
            return ()
        mutateViewToFront tinyWLView = do
            let server = (_tvServer tinyWLView)
            serverViews <- atomically $ readTVar (_tsViews server)
            let serverViewsUpdated = [tinyWLView] ++ (removeTinyWLViewFromList tinyWLView serverViews)
            atomically $ writeTVar (_tsViews server) serverViewsUpdated
        keyboardNotifyEnterIntoNewSurface ptrWlrSeat viewXdgSurface = do
          maybePtrWlrKeyboard          <- getSeatKeyboard ptrWlrSeat
          maybeXdgSurfaceAssociatedSurface <- xdgSurfaceGetSurface (_tvXdgSurface tinyWLView)
          case (maybePtrWlrKeyboard, maybeXdgSurfaceAssociatedSurface) of
              (Nothing, _)               -> putStrLn "Couldn't get keyboard!"
              (_, Nothing)               -> putStrLn "Couldn't get surface!"
              (Just ptrWlrKeyboard, Just surface) -> do
                (keycodes, numKeycodes) <- getKeyboardKeys ptrWlrKeyboard
                let modifiers = getModifierPtr ptrWlrKeyboard
                keyboardNotifyEnter ptrWlrSeat surface keycodes numKeycodes modifiers

-- | A wl_listener that (i) sets the TinyWLKeyboard as active in the seat (since
-- | wayland forces us to have one active keyboard as a time per seat) and (ii) sends
-- | the modifier keys to the client.
keyboardHandleModifiers :: TinyWLKeyboard -> WlListener TinyWLKeyboard
keyboardHandleModifiers tinyWLKeyboard = WlListener $ \ptrTinyWlKeyboard -> -- We're getting the same argument twice because this pointer scares me (don't understand where it's coming from)
  do let ptrWlrSeat     = (_tsSeat (_tkServer tinyWLKeyboard))
     let ptrInputDevice = (_tkDevice tinyWLKeyboard)

     seatSetKeyboard ptrWlrSeat ptrInputDevice -- Converts ptrInputDevice towlr_keyboard and makes it the active keyboard for the seat
     seatKeyboardNotifyModifiers ptrWlrSeat -- Sends modifiers to the client

     where seatKeyboardNotifyModifiers ptrWlrSeat = do
                maybePtrWlrKeyboard <- getSeatKeyboard ptrWlrSeat
                case maybePtrWlrKeyboard of
                    Nothing             -> putStrLn "Couldn't get keyboard!"
                    Just ptrWlrKeyboard -> do
                      (keycodes, numKeycodes) <- getKeyboardKeys ptrWlrKeyboard
                      let modifiers = getModifierPtr ptrWlrKeyboard
                      keyboardNotifyModifiers ptrWlrSeat modifiers

-- | This would normally be the function that is called to process, i.e., `<alt> +
-- | <key>` shortcuts at the compositor level; however, haskell-tinywl doesn't
-- | implement compositor-level keyboard shortcuts. If we did, we would make the
-- | second argument of type "Keysym" from XkbCommon.InternalTypes.
handleKeybinding :: TinyWLServer -> xkb_keysym_t -> IO (Bool)
handleKeybinding _ _ = do putStrLn "haskell-tinywl doesn't implement compositor-level keyboard shortcuts."
                          return False -- Returning False means "this function failed to find a valid combo to process this keybinding."

keyboardHandleKey :: TinyWLKeyboard -> WlListener WlrEventKeyboardKey
keyboardHandleKey tinyWLKeyboard = WlListener $ \ptrEventKey -> -- instance Storable: [[file:~/hsroots/src/Graphics/Wayland/WlRoots/Input/Keyboard.hsc::instance%20Storable%20EventKey%20where]] 
  {-
  do server <-(_tkServer tinyWLKeyboard)
     eventKey <- (peek ptrEventKey)
     let seat = (_tsSeat server)
     let eventKeyKeyCode = (keyCode eventKey)
     let eventKeyKeyCode8 = eventKeyKeyCode + 8;
     (_tkDevice tinyWLKeyboard)
  -}
     

-- getStateSyms :: KeyboardState -> CKeycode -> IO [Keysym]

  -- let eventKeyState = (state eventKey)
  -- let eventKeyTimeSec = (timeSec eventKey)
     -- ptrXkbKeysymTSysms <- 
     -- xkb_state_key_get_syms 

serverNewKeyboard :: TinyWLServer -> Ptr InputDevice
serverNewKeyboard = undefined
-- (_tkServer tinyWLKeyboard)
-- let seat = (_tsSeat server)

serverNewPointer :: TinyWLServer -> Ptr InputDevice
serverNewPointer = undefined

serverNewInput :: WlListener TinyWLServer
serverNewInput = undefined
-- let seat = (_tsSeat server)
seatRequestCursor :: WlListener TinyWLServer
seatRequestCursor = undefined
-- let seat = (_tsSeat server)

viewAt :: TinyWLView -> Double -> Double -> Ptr (Ptr WlrSurface) -> Double -> Double -> Bool
viewAt = undefined

desktopViewAt :: TinyWLServer -> Double -> Double -> Ptr (Ptr WlrSurface) -> Double -> Double -> TinyWLView
desktopViewAt  = undefined

processCursorMove :: TinyWLServer -> TimeSpec -> IO ()
processCursorMove = undefined

processCursorResize :: TinyWLServer -> TimeSpec -> IO ()
processCursorResize = undefined

processCursorMotion :: TinyWLServer -> TimeSpec -> IO ()
processCursorMotion = undefined
-- let seat = (_tsSeat server)

serverCursorMotion :: WlListener ()
serverCursorMotion = undefined

serverCursorMotionAbsolute :: WlListener ()
serverCursorMotionAbsolute = undefined

serverCursorButton :: WlListener ()
serverCursorButton = undefined
-- let seat = (_tsSeat server)

serverCursorAxis :: WlListener ()
serverCursorAxis = undefined
-- let seat = (_tsSeat server)

renderSurface  :: Ptr WlrSurface -> Int -> Int -> Ptr () -> IO ()
renderSurface  = undefined

outputFrame :: WlListener ()
outputFrame = undefined

serverNewOutput :: WlListener ()
serverNewOutput = undefined

xdgSurfaceMap :: WlListener ()
xdgSurfaceMap = undefined

xdgSurfaceUnmap :: WlListener ()
xdgSurfaceUnmap = undefined

xdgSurfaceDestroy :: WlListener ()
xdgSurfaceDestroy = undefined

beginInteractive :: TinyWLView -> TinyWLCursorMode -> Int -> IO ()
beginInteractive = undefined
-- let seat = (_tsSeat server)

xdgToplevelRequestMove :: WlListener ()
xdgToplevelRequestMove = undefined

xdgToplevelRequestResize :: WlListener ()
xdgToplevelRequestResize = undefined

serverNewXdgSurface :: WlListener ()
serverNewXdgSurface = undefined