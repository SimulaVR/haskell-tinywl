{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module TinyWL where

import           Debug.Trace
import           Control.Lens hiding (Context)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           Data.Maybe
import           Data.List

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           Text.XkbCommon.Keysym
import           Text.XkbCommon.KeyboardState
import           Text.XkbCommon.InternalTypes
import           Text.XkbCommon.Context
import           Text.XkbCommon.Keymap

import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.Internal.SpliceServerTypes
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
import           Graphics.Wayland.WlRoots.Input.Pointer
import           Graphics.Wayland.WlRoots.Cursor
import           Graphics.Wayland.WlRoots.Input.Buttons
import           Graphics.Wayland.WlRoots.Box
import           System.Clock
import           Control.Monad.Extra

-- Turn on inline-C for select calls that hsroots doesn't provide.
C.initializeTinyWLCtxAndIncludes

-- | TinyWL's C implementation uses Int for surface height/width and view location in output layout space; the rest of the time it uses Double.
-- | In Haskell we'll just always use Double but make things more type safe.
-- | TODO: Ensure this doesn't lead to unintended consequences, such as making a surface have non-integer volume or moving a surface into the middle of a pixel.
data OutputLayoutCoordinates = OutputLayoutCoordinates (Double, Double) -- Depending upon the context, we can be working with Int or Double.
data SurfaceLocalCoordinates = SurfaceLocalCoordinates (Double, Double) -- SurfaceLocalCoordinates are at the same scale as OutputLayoutCoordinates, despite being in Double
data SubSurfaceLocalCoordinates = SubSurfaceLocalCoordinates (Double, Double) -- "

data SurfaceDimension = SurfaceDimension (Int, Int)

data TinyWLCursorMode = TinyWLCursorPassthrough | TinyWLCursorMove |  TinyWLCursorResize

data TinyWLServer = TinyWLServer { _tsBackend              :: Ptr Backend
                                 , _tsRenderer             :: Ptr Renderer
                                 , _tsXdgShell             :: Ptr WlrXdgShell
                                 , _tsNewXdgSurface        :: WlListener WlrXdgSurface
                                 , _tsViews                :: TVar [TinyWLView]
                                 , _tsCursor               :: Ptr WlrCursor
                                 , _tsCursorManager        :: Ptr WlrXCursorManager
                                 , _tsCursorMotion         :: WlListener WlrEventPointerMotion
                                 , _tsCursorMotionAbsolute :: WlListener WlrEventPointerAbsMotion
                                 , _tsCursorButton         :: WlListener WlrEventPointerButton
                                 , _tsCursorAxis           :: WlListener WlrEventPointerAxis
                                 , _tsSeat                 :: Ptr WlrSeat
                                 , _tsNewInput             :: WlListener InputDevice
                                 , _tsRequestCursor        :: WlListener SetCursorEvent
                                 , _tsKeyboards            :: TVar [TinyWLKeyboard]
                                 , _tsCursorMode           :: TVar TinyWLCursorMode
                                 , _tsGrabbedView          :: TVar (Maybe TinyWLView)
                                 , _tsGrab                 :: TVar (Maybe SurfaceLocalCoordinates) -- The surface-local point at which a surface is grabbed.
                                 , _tsGrabWidth            :: TVar (Maybe Int)
                                 , _tsGrabHeight           :: TVar (Maybe Int)
                                 , _tsResizeEdges          :: TVar (Maybe Int)
                                 , _tsOutputLayout         :: Ptr WlrOutputLayout
                                 , _tsOutputs              :: TVar [TinyWLOutput]
                                 , _tsNewOutput            :: WlListener WlrOutput
                                 , _tsListenerTokens       :: [ListenerToken] -- Destroyed at the end of main
                                 }

data TinyWLOutput = TinyWLOutput { _toServer         :: TinyWLServer
                                 , _toWlrOutput      :: Ptr WlrOutput
                                 , _toFrame          :: WlListener WlrOutput
                                 , _toListenerTokens :: [ListenerToken] -- Not destroyed anywhere, but should be if Simula links distinct outputs to distinct views
                                 }

data TinyWLView = TinyWLView { _tvServer                  :: TinyWLServer
                             , _tvXdgSurface              :: Ptr WlrXdgSurface
                             , _tvMap                     :: WlListener WlrXdgSurface
                             , _tvUnmap                   :: WlListener WlrXdgSurface
                             , _tvDestroy                 :: WlListener WlrXdgSurface
                             , _tvRequestMove             :: WlListener MoveEvent
                             , _tvRequestResize           :: WlListener ResizeEvent
                             , _tvMapped                  :: TVar Bool
                             , _tvOutputLayoutCoordinates :: TVar OutputLayoutCoordinates -- Denotes top-left corner of view in OutputLayoutCoordinates
                             , _tvListenerTokens          :: [ListenerToken] -- Destroyed in the xdg surface destroy handler
                             }

data TinyWLKeyboard = TinyWLKeyboard { _tkServer         :: TinyWLServer
                                     , _tkDevice         :: Ptr InputDevice
                                     , _tkModifiers      :: WlListener ()
                                     , _tkKey            :: WlListener EventKey
                                     , _tkListenerTokens :: [ListenerToken] -- Not destroyed anywhere
                                     }

-- Used to move all of the data necessary to render a surface from the top-level
-- frame handler to the per-surface render function.
data RenderData = RenderData { _rdOutput   :: Ptr WlrOutput
                             , _rdRenderer :: Ptr Renderer
                             , _rdView     :: TinyWLView
                             , _rdWhen     :: TimeSpec
                             }

makeLenses ''TinyWLServer
makeLenses ''TinyWLOutput
makeLenses ''TinyWLView
makeLenses ''TinyWLKeyboard
makeLenses ''RenderData

-- | Helper function to convert Haskell timestamps to milliseconds (which wlroots expects) for C marshalling.
toMsec32 :: TimeSpec -> IO (Word32)
toMsec32 timeSpec = do
  let msec = fromIntegral $ toNanoSecs timeSpec `div` 1000000
  return msec

toTimeSpec :: (Integral a) => a -> TimeSpec
toTimeSpec word = (let nsec = (fromIntegral word) * 1000000 in fromNanoSecs nsec)

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
focusView tinyWLView ptrWlrSurface | trace ("focusView called with (xdgSurface, ptrWlrSurface): " ++ (show (tinyWLView ^. tvXdgSurface)) ++ ", " ++ (show ptrWlrSurface)) False = undefined
focusView tinyWLView ptrWlrSurface = do
  let ptrWlrSeat =_tsSeat (_tvServer tinyWLView)
  prevSurface <- getKeyboardFocus (getKeyboardState ptrWlrSeat)
  when (ptrWlrSurface /= prevSurface) $ do putStrLn $ "prevSurface (seat->keyboard_state.focused_surface) at the beginning of focusView: " ++ (show prevSurface)
                                           putStrLn $ "(ptrWlrSurface /= prevSurface: " ++ (show (ptrWlrSurface /= prevSurface))
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
                putStrLn $ "view->xdg_surface->surface (inside focusView): " ++ (show surface)
                (keycodes, numKeycodes) <- getKeyboardKeys ptrWlrKeyboard
                let modifiers = getModifierPtr ptrWlrKeyboard
                keyboardNotifyEnter ptrWlrSeat surface keycodes numKeycodes modifiers -- Should set focused_surface to surface, but doesn't.
                let seat' = toInlineC ptrWlrSeat
                let surface' = toInlineC surface
                seatKeyboardStateFocusedSurface <- [C.block| struct wlr_surface * {return $(struct wlr_seat * seat')->keyboard_state.focused_surface;} |]
                putStrLn $ "seat->keyboard_state.focused_surface (at the end of focusView): " ++ (show seatKeyboardStateFocusedSurface)

-- | A wl_listener that (i) sets the TinyWLKeyboard as active in the seat (since
-- | wayland forces us to have one active keyboard as a time per seat) and (ii) sends
-- | the modifier keys to the client.
keyboardHandleModifiers :: TinyWLKeyboard -> WlListener ()
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

-- | This function is the handler for key presses. It (i) sets the keyboard as the
-- | active one in the wayland seat and (ii) notifies the client about the keypress.
keyboardHandleKey :: TinyWLKeyboard -> WlListener EventKey
keyboardHandleKey tinyWLKeyboard = WlListener $ \ptrEventKey ->
  do event               <- (peek ptrEventKey) -- EventKey ∈ Storable
     let server          =  (_tkServer tinyWLKeyboard)
     let seat            =  (_tsSeat server)
     let device          =  (_tkDevice tinyWLKeyboard)
     let eventKeyState   =  (state event)
     let eventKeyTimeSec =  (timeSec event)
     let eventKeyKeyCode =  (keyCode event)

     -- Here is where we could check if the key event is a compositor-level shortcut
     -- i.e., an <alt> + <key> combo, and process it via `handleKeybinding`.

     seatSetKeyboard seat device
     keyboardNotifyKey seat eventKeyTimeSec eventKeyKeyCode eventKeyState

-- | This function "initializes" a wlr_input_device (second argument) into a
-- | wlr_keyboard. To do so it (i) constructs a bunch of XKB state to load into the
-- | keyboard; (ii) sets the keyboard's repeat info; (iii) attaches the
-- | keyboardHandleModifiers & keyboardHandleKey wl_listener's to the wlr_keyboard's
-- | "key" and "modifiers" signals respectively. It finally sets the keyboard as
-- | active (relative to our seat) and adds it to the head of the server's keyboard
-- | list.
serverNewKeyboard :: TinyWLServer -> Ptr InputDevice -> IO ()
serverNewKeyboard server device = do
  deviceType <- inputDeviceType device
  maybeXkbContext <- newContext defaultFlags

  case (deviceType, maybeXkbContext) of
       ((DeviceKeyboard ptrWlrKeyboard), (Just xkbContext)) -> mdo tokens <- handleKeyboard tinyWLKeyboard ptrWlrKeyboard xkbContext
                                                                   let tinyWLKeyboard = TinyWLKeyboard { _tkServer    = server                                   :: TinyWLServer
                                                                                                       , _tkDevice    = device                                   :: Ptr InputDevice
                                                                                                       , _tkModifiers = (keyboardHandleModifiers tinyWLKeyboard) :: WlListener ()
                                                                                                       , _tkKey       = (keyboardHandleKey       tinyWLKeyboard) :: WlListener EventKey
                                                                                                       , _tkListenerTokens = tokens                              :: [ListenerToken]
                                                                                                       }
                                                                   makeKeyboardActiveHead tinyWLKeyboard
                                                                   return ()
       _ -> putStrLn "Failed to get keyboard!"
  where handleKeyboard :: TinyWLKeyboard -> Ptr WlrKeyboard -> Context -> IO ([ListenerToken])
        handleKeyboard tinyWLKeyboard deviceKeyboard context = do
          maybeKeymap <- newKeymapFromNames context noPrefs
          case maybeKeymap of
               Nothing       -> putStrLn "Failed to get keymap!" >> return []
               (Just keymap) -> do setKeymapAndRepeatInfo deviceKeyboard keymap
                                   tokens <- handleSignals tinyWLKeyboard deviceKeyboard
                                   return tokens

        setKeymapAndRepeatInfo :: Ptr WlrKeyboard -> Keymap -> IO ()
        setKeymapAndRepeatInfo keyboard keymap = do
          withKeymap keymap (\keyMapC -> setKeymap keyboard keyMapC)
          let keyboard' = toInlineC keyboard
          [C.exp| void { wlr_keyboard_set_repeat_info($(struct wlr_keyboard * keyboard'), 25, 600) }|] -- hsroots doesn't provide this call.

        handleSignals :: TinyWLKeyboard -> Ptr WlrKeyboard -> IO ([ListenerToken])
        handleSignals tinyWLKeyboard keyboard = do
          let keyboardSignals = getKeySignals keyboard
          let keySignalKey' = (keySignalKey keyboardSignals)
          let keySignalModifiers' = (keySignalModifiers keyboardSignals)
          token1 <- addListener (_tkModifiers tinyWLKeyboard) keySignalModifiers' -- a = TinyWLKeyboard
          token2 <- addListener (_tkKey tinyWLKeyboard)       keySignalKey' -- a = EventKey

          let seat' = toInlineC (server ^. tsSeat)
          keyboardFocusChangeSignal' <- [C.exp| struct wl_signal * { &($(struct wlr_seat * seat')->keyboard_state.events.focus_change)}|]
          let keyboardFocusChangeSignal = (toC2HS keyboardFocusChangeSignal') :: Ptr (WlSignal ())

          token3 <- addListener (keyboardHandleFocusChange tinyWLKeyboard) keyboardFocusChangeSignal

          let tokens = [token1, token2, token3]
          return tokens

        makeKeyboardActiveHead :: TinyWLKeyboard -> IO ()
        makeKeyboardActiveHead tinyWLKeyboard = do
          seatSetKeyboard (_tsSeat server) (_tkDevice tinyWLKeyboard)
          keyboardList <- atomically $ readTVar (_tsKeyboards server)
          atomically $ writeTVar (_tsKeyboards server) ([tinyWLKeyboard] ++ keyboardList)

        keyboardHandleFocusChange :: TinyWLKeyboard -> WlListener ()
        keyboardHandleFocusChange tinyWLKeyboard = WlListener $ \ptr -> do
          ns <- [C.block| struct wlr_surface * {
                                  struct wlr_seat_keyboard_focus_change_event * event;
                                  event = (struct wlr_seat_keyboard_focus_change_event *) $(void * ptr);
                                  //printf("seat: %p\nold_surface: %p\nnew_surface: %p", event->seat, event->old_surface, event->new_surface);
                                  //printf("new_surface: %p", event->new_surface);
                                  return event->new_surface;
              } |]
          putStrLn $ "New surface focused on: " ++ (show ns)

-- | Here we just wrap a call to wlr_cursor_attach_input_device. The reason this function is so simple is that we all pointer handling is proxied through wlr_cursor by default. VR inputs will likely involve studying wlr_cursor in detail to see how it handles motion events.
serverNewPointer :: TinyWLServer -> Ptr InputDevice -> IO ()
serverNewPointer tinyWLServer device = do
  let cursor = (_tsCursor tinyWLServer)
  attachInputDevice cursor device
-- | This wl_listener is called when TinyWL gets a new wlr_input_device. We (i)
-- | inspect the wlr_input_device to see whether it is a keyboard or a pointer,
-- | passing it to serverNew* accordingly; (ii) set the wlr_seat's "capabilities" as
-- | either "mouse" or "mouse + keyboard", depending upon whether the server's
-- | keyboard list is empty or not.
serverNewInput :: TinyWLServer -> WlListener InputDevice
serverNewInput tinyWLServer = WlListener $ \device ->
  do deviceType <- inputDeviceType device
     let seat = (_tsSeat tinyWLServer)
     return ()
     case deviceType of
         (DeviceKeyboard _) -> (serverNewKeyboard tinyWLServer device)
         (DevicePointer  _) -> (serverNewPointer tinyWLServer  device)
         _                  -> putStrLn "TinyWL does not know how to handle this input type."
     -- Note that Graphics.Wayland.Internal.SpliceServerTypes code generates the following:
     --   newtype SeatCapability = SeatCapability GHC.Types.Int
     -- Now see: https://github.com/swaywm/hsroots/blob/f9b07af96dff9058a3aac59eba5a608a91801c0a/src/Graphics/Wayland/WlRoots/Input.hsc#L48
     let mouseCapability = SeatCapability (deviceTypeToInt (DevicePointer nullPtr))
     let keyboardCapability = SeatCapability (deviceTypeToInt (DeviceKeyboard nullPtr))
     let mouseCapabilities = [mouseCapability] :: [SeatCapability]
     let allCapabilities = [mouseCapability, keyboardCapability] :: [SeatCapability]
     keyboardList <- atomically $ readTVar (_tsKeyboards tinyWLServer)
     let capabilities = if (null keyboardList)
                            then mouseCapabilities
                            else allCapabilities
     setSeatCapabilities seat capabilities

-- | When a seat raises wlr_seat_pointer_request_set_cursor_event (when a client
-- | provides cursor image), this handler (which just wraps wlr_cursor_set_surface)
-- | is called.
seatRequestCursor :: TinyWLServer -> WlListener SetCursorEvent
seatRequestCursor tinyWLServer = WlListener $ \ptrSetCursorEvent ->
  do setCursorEvent <- peek ptrSetCursorEvent -- [[file:~/hsroots/src/Graphics/Wayland/WlRoots/Seat.hsc::instance%20Storable%20SetCursorEvent%20where]] 
     let clientRequester = (toInlineC $ unSeatClient $ seatCursorSurfaceClient setCursorEvent) :: Ptr C'WlrSeatClient
     let seat = toInlineC (_tsSeat tinyWLServer)
     focusedClient <- [C.exp| struct wlr_seat_client * { $(struct wlr_seat * seat)->pointer_state.focused_client } |] -- hsroots doesn't provide access to this data structure AFAIK
     let cursor = (_tsCursor tinyWLServer)
     let surface = seatCursorSurfaceSurface setCursorEvent
     let hotspotX = seatCursorSurfaceHotspotX setCursorEvent
     let hotspotY = seatCursorSurfaceHotspotY setCursorEvent

     when (clientRequester == focusedClient) $ setCursorSurface cursor surface hotspotX hotspotY -- Since this event can be called by any client, we first make sure the client that triggered it actually has seat focus

-- | This function takes a pair of output layout coordinates (for a single view) and
-- | inspects if there are any XDG toplevel "subsurfaces" at those coordinates (this
-- | could either be a true "subsurface" -- like a popup -- or the trivial subsurface
-- | [the parent surface associated with the view]). We return the wlr_surface (if there is one)
-- | associated with the "subsurface", and convert the output layout coordinates to the
-- | corresponding subsurface coordinates relative to the subsurface.
viewAt :: TinyWLView -> OutputLayoutCoordinates -> IO (Maybe (Ptr WlrSurface, SubSurfaceLocalCoordinates))
viewAt tinyWLView (OutputLayoutCoordinates (lx, ly)) = do                                                                     -- (lx, ly) denote the cursor's position in output layout space
  OutputLayoutCoordinates (topleftX, topleftY) <- atomically $ readTVar (_tvOutputLayoutCoordinates tinyWLView)                                  -- (topleftX, topleftY) denotes the view's top-left corner in output layout space
  let SurfaceLocalCoordinates (sx, sy) = SurfaceLocalCoordinates (lx - topleftX, ly - topleftY) -- (sx, sy) denote the cursor's position relative to the tinyWLView's surface space

  -- In tinywl.c, there is a dangling `struct wlr_surface_state *state = &view->xdg_surface->surface->current;` whose purpose is unclear.

  let xdgSurface = (_tvXdgSurface tinyWLView)
  maybeSubSurface <- xdgSurfaceAt xdgSurface sx sy
  case maybeSubSurface of
       Nothing                     -> return $ Nothing
       Just (subSurface, ssx, ssy) -> return $ Just (subSurface, SubSurfaceLocalCoordinates (ssx, ssy))                       -- (ssx, ssy) denote the cursor's position relative to the subsurface's space.

-- | This function is similar to `viewAt`, except it inspects *all* of the server's
-- | views for subsurfaces and returns a view. This relies on the server's list of
-- | views being ordered from top-to-bottom (and this in turn explains why in, i.e.,
-- | `focusView` we ensure to mutate the active view to the head of our server's list
-- | of views).
desktopViewAt :: TinyWLServer -> OutputLayoutCoordinates -> IO (Maybe TinyWLView)
desktopViewAt  tinyWLServer outputLayoutCoordinates = do
  listOfViews             <- atomically $ readTVar (_tsViews tinyWLServer)
  maybeViewWithSubsurface <- findM hasSubsurface listOfViews
  return maybeViewWithSubsurface
  where hasSubsurface :: TinyWLView -> IO (Bool)
        hasSubsurface tinyWLView = do
          maybeSurfaceData <- viewAt tinyWLView outputLayoutCoordinates 
          case maybeSurfaceData of
               Just _  -> return True
               Nothing -> return False

-- | This function mutates the server's grabbed view location (specifically: the
-- | location of its top-left corner in output layout space) based on (i) the
-- | point at which the view was grabbed (in surface local coordintaes) and (ii)
-- | the point at which the cursor is currently located (in output layout space).
-- | Keeping track of all of these coordinate systems can be confusing so to make
-- | things as easy as possible we add type annotations even when not strictly
-- | required.
-- | NOTE: The second argument isn't used in the C implementation.
processCursorMove :: TinyWLServer -> TimeSpec -> IO ()
processCursorMove tinyWLServer _ = do
   maybeGrabbedView <- atomically $ readTVar (tinyWLServer ^. tsGrabbedView)
   maybeSurfaceGrabPoint <- atomically $ readTVar (tinyWLServer ^. tsGrab)
   case (maybeGrabbedView, maybeSurfaceGrabPoint) of
     (Just grabbedView, Just (SurfaceLocalCoordinates (surfaceGrabPointSX, surfaceGrabPointSY))) -> do
              grabbedViewTopLeft@(OutputLayoutCoordinates (grabbedViewLX, grabbedViewLY)) <- atomically $ readTVar (grabbedView ^. tvOutputLayoutCoordinates)
              let cursor = (_tsCursor tinyWLServer)
              cursorLX <- getCursorX cursor
              cursorLY <- getCursorY cursor
              let cursorPosition = OutputLayoutCoordinates (cursorLX, cursorLY)
              let newGrabbedViewTopLeft = OutputLayoutCoordinates (cursorLX - surfaceGrabPointSX, cursorLY - surfaceGrabPointSY)

              -- Nested TVars require us to mutate twice :(
              atomically $ writeTVar (grabbedView ^. tvOutputLayoutCoordinates) newGrabbedViewTopLeft
              atomically $ writeTVar (tinyWLServer ^. tsGrabbedView) (Just grabbedView)
     (_, _) -> putStrLn "No grabbed view for us to move!"

-- | Since VR requires specialized treatment of surface resizing, I'm waiting to
-- | implement this until later.
processCursorResize :: TinyWLServer -> TimeSpec -> IO ()
processCursorResize _ _ = do
  putStrLn "Window resizing is not yet implemented in haskell-tinywl!"

-- | This function inspects the server's cursor mode, and either hands over to
-- | processCursorMove or processCursorResize, or handles movement via (i) finding
-- | the view under the cursor and passing responsibility to client or, if there is
-- | no view, (ii) drawing the cursor image at the compositor level. There are some
-- | subtleties in this function that may impact VR inputs, namely: (i) when a cursor
-- | notifies a surface that it has entered it, it is automatically given focus and
-- | (ii) when the cursor is not over any view, we repeatedly emit
-- | "pointerClearFocus" events to ensure that button events aren't sent to surfaces
-- | that previously had focus.
processCursorMotion :: TinyWLServer -> TimeSpec -> IO ()
processCursorMotion tinyWLServer timeSpec = do
  cursorMode <- atomically $ readTVar (_tsCursorMode tinyWLServer)
  case cursorMode of
       TinyWLCursorMove -> processCursorMove tinyWLServer timeSpec
       TinyWLCursorResize -> processCursorResize tinyWLServer timeSpec
       TinyWLCursorPassthrough -> serverPassthroughMotion
  where serverPassthroughMotion :: IO ()
        serverPassthroughMotion = do
          let seat = (_tsSeat tinyWLServer)
          let cursor = (_tsCursor tinyWLServer)
          let cursorManager = (_tsCursorManager tinyWLServer)
          cursorLX <- getCursorX cursor
          cursorLY <- getCursorY cursor
          let cursorPosition = OutputLayoutCoordinates (cursorLX, cursorLY)
          maybeViewAtPoint <- desktopViewAt tinyWLServer cursorPosition
          case maybeViewAtPoint of
                Nothing -> do
                  xCursorSetImage cursorManager "left_ptr" cursor -- If there's no view, then the server is responsible for drawing the cursor image
                  pointerClearFocus seat -- Clear the pointer focus so future buton events aren't set to the last client that had focus
                Just viewAtPoint -> do
                  (subSurfaceAtPoint, SubSurfaceLocalCoordinates (ssx, ssy))  <- fromJust <$> viewAt viewAtPoint cursorPosition
                  surfaceMotion subSurfaceAtPoint ssx ssy -- The term "subsurface" here is somewhat misleading; this could be either be something like a popup or an ordinary surface associated with a view
        surfaceMotion :: Ptr WlrSurface -> Double -> Double -> IO ()
        surfaceMotion subSurfaceAtPoint ssx ssy = do
            let seat = (_tsSeat tinyWLServer)
            let seat' = toInlineC seat
            lastFocusedSurface' <- [C.exp| struct wlr_surface * { $(struct wlr_seat * seat')->pointer_state.focused_surface } |] -- hsroots doesn't provide access to this data structure AFAIK
            let lastFocusedSurface = toC2HS lastFocusedSurface'
            let focusChanged = (lastFocusedSurface /= subSurfaceAtPoint)
            msec32 <- toMsec32 timeSpec

            -- Either tell the client we just entered the surface, or that we moved within the surface.
            when focusChanged       $ pointerNotifyEnter seat subSurfaceAtPoint ssx ssy -- This automatically gives the surface "pointer focus", which is distinct from keyboard focus. TODO: Ensure this doesn't cause issue with VR, where we have two controllers potentially fighting for focus.
            when (not focusChanged) $ pointerNotifyMotion seat msec32 ssx ssy

-- | The cursor forwards this event when a pointer emits _relative_ motion (i.e., dx
-- | & dy). WlRoots automatically ensures relative pointer motion can't cause a
-- | pointer to escape the boundaries of our output layout.

-- | I'm not sure how VR motion will handle relative motion. Need we simulate it for
-- | proper VR input functionality? It appears from glancing at the source that this
-- | function ultimately wraps `wlr_output_cursor_move`, which just
-- | changes cursor state (and doesn't seem to indicate we need to simulate relative
-- | cursor movement in VR). See also `serverCursorMotionAbsolute`.
serverCursorMotion :: TinyWLServer -> WlListener WlrEventPointerMotion
serverCursorMotion tinyWLServer = WlListener $ \ptrEventPointerMotion -> do
  eventPointerMotion     <- peek ptrEventPointerMotion
  let eventDevice        = eventPointerMotionDevice eventPointerMotion
  let eventTimeMSec      = eventPointerMotionTime eventPointerMotion
  let eventDeltaX        = eventPointerMotionDeltaX eventPointerMotion
  let eventDeltaY        = eventPointerMotionDeltaY eventPointerMotion
  let cursor             = (tinyWLServer ^. tsCursor)
  moveCursor cursor (Just eventDevice) eventDeltaX eventDeltaY -- Causes _relative_ pointer motion, and I *think* ultimately just modifies *cursor state*
  processCursorMotion tinyWLServer (toTimeSpec eventTimeMSec) -- Wraps our motion handlers and emits events to clients; see above.

-- | Same as serverCursorMotion except sends cursor to absolute position (in output
-- | layout space). This is normally used when running our compositor in, i.e., an
-- | X11 window (say when the X11 cursor moves into our compositor from one of the edges);
-- | however, it's likely we'll use this heavily for VR inputs that constantly emit
-- | absolute pointer motion.
serverCursorMotionAbsolute :: TinyWLServer -> WlListener WlrEventPointerAbsMotion
serverCursorMotionAbsolute tinyWLServer = WlListener $ \ptrEventPointerMotionAbs -> do
  eventPointerMotionAbs                <- peek ptrEventPointerMotionAbs
  let OutputLayoutCoordinates (lx, ly) = OutputLayoutCoordinates (eventPointerAbsMotionX eventPointerMotionAbs,
                                                                  eventPointerAbsMotionY eventPointerMotionAbs)
  let device                           = eventPointerAbsMotionDevice eventPointerMotionAbs
  let eventTimeMSec32                  = eventPointerAbsMotionTime eventPointerMotionAbs
  let cursor                           = (tinyWLServer ^. tsCursor)
  warpCursorAbs cursor (Just device) (Just lx) (Just ly) -- Forces cursor to output layout coordinates (lx, ly)
  processCursorMotion tinyWLServer (toTimeSpec eventTimeMSec32) -- Wraps our motion handlers and emits events to clients; see above.
  -- warpCursorAbs :: Ptr WlrCursor -> Maybe (Ptr InputDevice) -> Maybe Double -> Maybe Double -> IO ()
  -- moveCursor cursor (Just eventDevice) eventDeltaX eventDeltaY -- Causes _relative_ pointer motion, and I *think* ultimately just modifies *cursor state*
  return ()

-- | This event is forwarded by the cursor when a pointer emits a button event.
serverCursorButton :: TinyWLServer -> WlListener WlrEventPointerButton
serverCursorButton tinyWLServer = WlListener $ \ptrEventPointerButton -> do
  let seat = (tinyWLServer ^. tsSeat)
  eventPointerButton <- peek ptrEventPointerButton
  let device = eventPointerButtonDevice eventPointerButton :: Ptr InputDevice
  let time32 = eventPointerButtonTime   eventPointerButton :: Word32
  let button =  eventPointerButtonButton eventPointerButton :: Word32
  let buttonState =  eventPointerButtonState  eventPointerButton :: ButtonState
  let cursor = (tinyWLServer ^. tsCursor)
  cursorLX <- getCursorX cursor
  cursorLY <- getCursorY cursor
  let cursorPosition = OutputLayoutCoordinates (cursorLX, cursorLY)
  maybeViewAtCursorPoint <- desktopViewAt tinyWLServer cursorPosition

  pointerNotifyButton seat time32 button buttonState -- Notify the client with pointer focus that a button press has ocurred.
  case (buttonState, maybeViewAtCursorPoint) of
       (ButtonReleased, _)                     -> do atomically $ writeTVar (tinyWLServer ^. tsCursorMode) TinyWLCursorPassthrough
       (ButtonPressed, Just viewAtCursorPoint) ->  do (surfaceAtCursorPoint, _) <- fromJust <$> viewAt viewAtCursorPoint cursorPosition
                                                      focusView viewAtCursorPoint surfaceAtCursorPoint -- Ensure the keyboard has focus if there's a view at point
       (ButtonPressed, Nothing) -> return ()


-- | This event is forwarded by the cursor when a pointer emits an "axis event". for
-- | example when you move the scroll wheel (we'll use this VR touchpad scrolling).
serverCursorAxis :: TinyWLServer -> WlListener WlrEventPointerAxis
serverCursorAxis tinyWLServer = WlListener $ \ptrEventPointerAxis -> do
  let seat            = (tinyWLServer ^. tsSeat)
  event               <- peek ptrEventPointerAxis
  let device          = eventPointerAxisDevice event      :: Ptr InputDevice
  let time32          = eventPointerAxisTime event        :: Word32
  let axisSource      = eventPointerAxisSource event      :: AxisSource
  let axisOrientation = eventPointerAxisOrientation event :: AxisOrientation
  let axisDeltaValue  = eventPointerAxisDelta event       :: Double
  let axisDiscrete    = eventPointerAxisDiscrete event    :: Int32
  pointerNotifyAxis seat time32 axisOrientation axisDeltaValue axisDiscrete

-- | Takes a surface of dimension (sx, sy) alongside a RenderData (output, renderer,
-- | TinyWLView, and a timespec) and (i) generates a matrix transform to (ii) render
-- | the surface and finally (iii) tell the client that we're done rendering so it
-- | can start drawing the next frame. This particular function uses a wlr_box (a
-- | rectangle with a point inside it) to help generate the matrix (the rectangle
-- | represents an output with the surface location in output-local coordinates).
-- | Since we're going to use our own custom VR rendering with Simula/Godot, this is
-- | just an inline-C function.
renderSurface  :: Ptr WlrSurface -> SurfaceDimension -> RenderData -> IO ()
renderSurface  ptrWlrSurface surfaceDimension@(SurfaceDimension (sx, sy)) renderData = do
  let tinyWLView = (renderData ^. rdView)
  viewLocation@(OutputLayoutCoordinates (lx, ly)) <- atomically $ readTVar (tinyWLView ^. tvOutputLayoutCoordinates)

  -- Inline-C types
  let ptrWlrOutputLayout' = toInlineC (_tsOutputLayout (tinyWLView ^. tvServer)) :: Ptr C'WlrOutputLayout
  let ptrWlrSurface'      = toInlineC ptrWlrSurface                              :: Ptr C'WlrSurface
  let ptrWlrOutput'       = toInlineC (renderData ^. rdOutput)                   :: Ptr C'WlrOutput
  let (sx', sy')          = (fromIntegral sx, fromIntegral sy)                   :: (CInt, CInt)
  let (lx', ly')          = (realToFrac lx, realToFrac ly)                       :: (CDouble, CDouble)
  let renderer'           = toInlineC (renderData ^. rdRenderer)

  -- Dangerous memory allocation, but we free it at the end of this function.
  let timeSpec            = (renderData ^. rdWhen)                               :: TimeSpec
  ptrTimeSpec             <- malloc                                              :: IO (Ptr TimeSpec)
  poke                    ptrTimeSpec timeSpec

  [C.block| void {struct wlr_output        * output        = $(struct wlr_output        * ptrWlrOutput');
                  struct wlr_output_layout * output_layout = $(struct wlr_output_layout * ptrWlrOutputLayout');
                  struct wlr_renderer      * renderer      = $(struct wlr_renderer      * renderer');
                  struct wlr_surface       * surface       = $(struct wlr_surface       * ptrWlrSurface');
                  struct timespec          * time_spec     = $(struct timespec          * ptrTimeSpec);
                  int                        sx            = $(int                        sx');
                  int                        sy            = $(int                        sy');
                  double                     lx            = $(double                     lx');
                  double                     ly            = $(double                     ly');

                  //1. Get texture
                  struct wlr_texture *texture = wlr_surface_get_texture(surface);
                  if (texture == NULL) {
                          return;
                  }
                  double ox = 0, oy = 0;

                  //2. Construct transform from surface->output
                  wlr_output_layout_output_coords(
                                  output_layout, output, &ox, &oy);
                  ox += lx + sx, oy += ly + sy;

                  struct wlr_box box = {
                          .x = ox * output->scale,
                          .y = oy * output->scale,
                          .width = surface->current.width * output->scale,
                          .height = surface->current.height * output->scale,
                  };

                  float matrix[9];
                  enum wl_output_transform transform =
                          wlr_output_transform_invert(surface->current.transform);
                  wlr_matrix_project_box(matrix, &box, transform, 0,
                          output->transform_matrix);

                  //3. Render
                  wlr_render_texture_with_matrix(renderer, texture, matrix, 1);

                  //4. Tell the client we're done rendering so it can start drawing the next frame
                  wlr_surface_send_frame_done(surface, time_spec); //time_spec references aren't used after this call, so we can safely delete the memory
               } |]
  free ptrTimeSpec
  return ()


-- | This is a helper function to generate typedef void
-- |   (*wlr_surface_iterator_func_t)(struct wlr_surface *surface, int sx, int
-- |   sy, void *data); used in calls to, i.e., wlr_xdg_surface_for_each_surface.
-- | Rather than uses the void pointer to jam a RenderData into it, we pass a
-- | RenderData into the first argument and close over it (otherwise we'd have to
-- | implement a bunch of Storable instances).
renderSurfaceIteratorClosure :: RenderData -> Ptr C'WlrSurface -> CInt -> CInt -> Ptr () -> IO ()
renderSurfaceIteratorClosure renderData ptrWlrSurface' sx sy _ = do
  let ptrWlrSurface = toC2HS ptrWlrSurface'
  renderSurface ptrWlrSurface (SurfaceDimension ((fromIntegral sx), (fromIntegral sy))) renderData

-- | This function is called every time an output is ready to display a frame.
-- | TODO: FPS indicator (& compare to tinywl.c).
outputFrame :: TinyWLOutput -> WlListener WlrOutput
outputFrame tinyWLOutput = WlListener $ \_ -> do
  let ptrRenderer = (tinyWLOutput ^. toServer ^. tsRenderer)
  now <- getTime Realtime
  let wlrOutput = (tinyWLOutput ^. toWlrOutput)
  maybeOutputCurrent <- makeOutputCurrent wlrOutput -- If this returns 0, then we shouldn't anything
  case maybeOutputCurrent of
       Nothing -> putStrLn "Unable to make output current!" -- If we can't make the output current, then do nothing
       _       -> renderViews wlrOutput ptrRenderer now

  where renderViews:: Ptr WlrOutput -> Ptr Renderer -> TimeSpec -> IO ()
        renderViews wlrOutput ptrRenderer now = do
            reverseViewList <- reverse <$> (atomically $ readTVar (tinyWLOutput ^. toServer ^. tsViews))
            let reverseRendererList = fmap (\view -> RenderData { _rdOutput = wlrOutput
                                                                , _rdRenderer = ptrRenderer
                                                                , _rdView = view
                                                                , _rdWhen = now }) reverseViewList
            let purple      = Color 0.6 0.2 0.8 1
            (width, height) <- effectiveResolution wlrOutput -- I believe this changes if you, i.e., rotate an output
            let wlrOutput' = toInlineC wlrOutput :: Ptr C'WlrOutput
            rendererBegin ptrRenderer (fromIntegral width) (fromIntegral height)
            rendererClear ptrRenderer purple
            mapM_ renderView reverseRendererList -- render each surface in reverse order so they appear properly overlapped

            -- This is needed in case hardware cursors are not supported by your
            -- hardware; it renders cursor on top of your views at the hardware
            -- level; it won't do anything if hardware cursors are enabled.
            [C.exp| void {wlr_output_render_software_cursors($(struct wlr_output * wlrOutput'), $(void * nullPtr))} |]
            rendererEnd ptrRenderer
            swapOutputBuffers wlrOutput Nothing Nothing -- Swapping buffers shows the actual frame on screen
            return ()

        -- This function assumes its called in between `rendererBegin` and `rendererEnd`
        renderView :: RenderData -> IO ()
        renderView renderData = do
            -- This might not work if `wlr_xdg_surface_for_each_surface` somehow
            -- needs the `Ptr ()` (which in the C implementation has the
            -- render_data).
            renderSurfaceIteratorT <- $(C.mkFunPtr [t| Ptr C'WlrSurface -> CInt -> CInt -> Ptr () -> IO () |]) $ renderSurfaceIteratorClosure renderData :: IO C'WlrSurfaceIteratorFuncT
            let tinyWLView = renderData ^. rdView
            let viewXdgSurface = toInlineC (tinyWLView ^. tvXdgSurface)
            isMapped <- atomically $ readTVar (tinyWLView ^. tvMapped)
            case isMapped of
                  False -> return () -- Don't render unmapped surfaces
                  True -> [C.exp| void { wlr_xdg_surface_for_each_surface($(struct wlr_xdg_surface * viewXdgSurface), $(wlr_surface_iterator_func_t renderSurfaceIteratorT), $(void * nullPtr))}|] -- Calls renderSurfaceIteratorT for each surface among xdg_surface's toplevel and popups (hsroots doesn't provide).

            freeHaskellFunPtr renderSurfaceIteratorT
-- | This is an event handler raised by the backend when a new output (i.e.,
-- | display or monitor) becomes available.
serverNewOutput :: TinyWLServer -> WlListener WlrOutput
serverNewOutput tinyWLServer = WlListener $ \ptrWlrOutput -> mdo
    -- Sets the outputs (width, height, refresh rate) which depends on your hardware.
    -- This is only necessary for some backends (i.e., DRM+KMS). Here we just pick
    -- the first mode supported in the list retrieved.
    setOutputModeAutomatically ptrWlrOutput

    let tinyWLOutput = TinyWLOutput { _toServer = tinyWLServer
                                    , _toWlrOutput = ptrWlrOutput
                                    , _toFrame = (outputFrame tinyWLOutput)
                                    , _toListenerTokens = tokens
                                    }
    let signal = outSignalFrame (getOutputSignals ptrWlrOutput)
    token <- addListener (tinyWLOutput ^. toFrame) signal
    let tokens = [token]

    -- Add output to head of server's output list
    outputsList  <- atomically $ readTVar (tinyWLServer ^. tsOutputs)
    let outputsListNew = tinyWLOutput:outputsList
    atomically $ writeTVar (tinyWLServer ^. tsOutputs) outputsListNew

    let ptrOutputLayout = tinyWLServer ^. tsOutputLayout

    -- This function automatically adds output to the layout in the left-to-right
    -- order in which they appear. We might have to adjust this in Simula if we
    -- are to treat each surface sprite as as its own output.
    addOutputAuto ptrOutputLayout ptrWlrOutput

    -- Adds the wl_output global to the display, which Wayland clients can use to
    -- find out information about this output (such as DPI, scale factor,
    -- manufacturerer, etc).
    createOutputGlobal ptrWlrOutput

    where setOutputModeAutomatically :: Ptr WlrOutput -> IO ()
          setOutputModeAutomatically ptrWlrOutput = do
            hasModes' <- hasModes ptrWlrOutput
            when hasModes' $ do modes <- getModes ptrWlrOutput
                                let headMode = head modes -- We might want to reverse this list first to mimic C implementation
                                setOutputMode headMode ptrWlrOutput
                                return ()

-- | Called when the surface is "mapped" (i.e., "ready to display
-- | on-screen").
xdgSurfaceMap :: TinyWLView -> WlListener WlrXdgSurface
xdgSurfaceMap tinyWLView = WlListener $ \_ -> do
  maybeSurface <- xdgSurfaceGetSurface (tinyWLView ^. tvXdgSurface)

  -- Set the XDG surface as mapped
  atomically $ writeTVar (tinyWLView ^. tvMapped) True

  -- Then give it keyboard focus
  case maybeSurface of
    Nothing -> return ()
    Just surface -> focusView tinyWLView surface
  return ()

-- | Called when the surface is "unmapped", and should no longer be shown.
xdgSurfaceUnmap :: TinyWLView -> WlListener WlrXdgSurface
xdgSurfaceUnmap tinyWLView = WlListener $ \_ -> do
  atomically $ writeTVar (tinyWLView ^. tvMapped) False

-- | Called when the surface is destroyed, and should never be shown again.
xdgSurfaceDestroy :: TinyWLView -> WlListener WlrXdgSurface
xdgSurfaceDestroy tinyWLView = WlListener $ \_ -> do
  mapM_ freeListenerToken (tinyWLView ^. tvListenerTokens)

  viewList <- atomically $ readTVar (tinyWLView ^. tvServer ^. tsViews)
  let cleansedViewList = removeTinyWLViewFromList tinyWLView viewList

  -- Remove the view from the server's list of views
  atomically $ writeTVar (tinyWLView ^. tvServer ^. tsViews) cleansedViewList 

-- | This is the function called when a user starts to move or resize a surface.
-- | This ultimately causes the compositor to stop propagating pointer events to
-- | clients and instead consume them itself to move/resize windows. Locally, it
-- | mutates the server to signal its in the middle of a grab/resize.
beginInteractive :: TinyWLView -> TinyWLCursorMode -> Int -> IO ()
beginInteractive tinyWLView cursorMode edges = do
  let tinyWLServer = tinyWLView ^. tvServer
  let seat' = toInlineC (tinyWLServer ^. tsSeat)
  lastFocusedSurface' <- [C.exp| struct wlr_surface * { $(struct wlr_seat * seat')->pointer_state.focused_surface } |] -- hsroots doesn't provide access to this data structure AFAIK
  let lastFocusedSurface = toC2HS lastFocusedSurface'
  let viewXdgSurface = (tinyWLView ^. tvXdgSurface)
  maybeViewSurface <- xdgSurfaceGetSurface viewXdgSurface

  case maybeViewSurface of
       Nothing -> return ()
       (Just viewSurface) -> do let  viewIsntFocused = (lastFocusedSurface /= viewSurface)
                                case viewIsntFocused of
                                    True -> return () -- Ignore unfocused clients that try to resize/move surfaces
                                    False -> mutateServer tinyWLServer viewXdgSurface viewSurface

  where mutateServer tinyWLServer viewXdgSurface viewSurface = do
          let cursor = (tinyWLServer ^. tsCursor)
          cursorLX <- getCursorX cursor
          cursorLY <- getCursorY cursor
          let cursorPosition = OutputLayoutCoordinates (cursorLX, cursorLY)
          viewPosition@(OutputLayoutCoordinates (viewLX, viewLY)) <- atomically $ readTVar (tinyWLView ^. tvOutputLayoutCoordinates)

          -- I do *not* conceptually understand what the point of this wlr_box is.
          geoBox <- getGeometry viewXdgSurface -- I believe this just makes a wlr_box with the same geometry as the viewXdgSurface
          let geoBoxX = (boxX geoBox) -- Source makes it look like this is just the width of the surface?
          let geoBoxY = (boxY geoBox) -- "
          let geoBoxWidth = (boxWidth geoBox)
          let geoBoxHeight = (boxHeight geoBox)

          -- Mutate server state to reflect that we're in the middle of a grab
          atomically $ writeTVar (tinyWLServer ^. tsGrabbedView) (Just tinyWLView)
          atomically $ writeTVar (tinyWLServer ^. tsCursorMode) cursorMode

          case cursorMode of
              TinyWLCursorMove -> atomically $ writeTVar (tinyWLServer ^. tsGrab) (Just (SurfaceLocalCoordinates ( (cursorLX - viewLX) , (cursorLY - viewLY) )))   -- Mutate the grab coordinates to their natural surface-local position
              _                -> atomically $ writeTVar (tinyWLServer ^. tsGrab) (Just (SurfaceLocalCoordinates ( (cursorLX + (fromIntegral geoBoxX)) , (cursorLY + (fromIntegral geoBoxY)) ))) -- Totally unclear what this is doing; why not do as above??

          -- More server mutation
          atomically $ writeTVar (tinyWLServer ^. tsGrabWidth) (Just geoBoxWidth)
          atomically $ writeTVar (tinyWLServer ^. tsGrabHeight) (Just geoBoxHeight)
          atomically $ writeTVar (tinyWLServer ^. tsResizeEdges) (Just edges)

-- | Raised when a client requests interactive move (typically because the user
-- | clicked on their client-side decorations).
xdgToplevelRequestMove :: TinyWLView -> WlListener MoveEvent
xdgToplevelRequestMove tinyWLView = WlListener $ \_ -> do
  beginInteractive tinyWLView TinyWLCursorMove 0

-- | Raised when a client requests interactive resize (typically because the user
-- | clicked on their client-side decorations).
xdgToplevelRequestResize :: TinyWLView -> WlListener ResizeEvent
xdgToplevelRequestResize tinyWLView = WlListener $ \ptrResizeEvent -> do
  resizeEvent <- peek ptrResizeEvent
  -- let surface = resizeEvtSurface resizeEvent :: Ptr WlrXdgSurface
  -- let seat= resizeEvtSeat    resizeEvent     :: Ptr WlrSeatClient
  -- let serial = resizeEvtSerial  resizeEvent  :: Word32
  let edges = resizeEvtEdges   resizeEvent   :: Word32
  beginInteractive tinyWLView TinyWLCursorResize (fromIntegral edges)

-- | This event is raised when wlr_xdg_shell receives a new xdg surface from a
-- | client, either a toplevel (application window) or popup.
serverNewXdgSurface :: TinyWLServer -> WlListener WlrXdgSurface
serverNewXdgSurface tinyWLServer = WlListener $ \ptrWlrXdgSurface -> do
  maybeTopLevel <- getXdgToplevel ptrWlrXdgSurface
  case maybeTopLevel of
       Nothing         -> return () -- If the surface is, i.e., a popup or has no XDG "role", then we don't do anything.
       (Just topLevel) -> createServerView ptrWlrXdgSurface topLevel
  where
        -- createServerView creates a new TinyWLView, connects its WlListeners
        -- to the proper signals, and adds it to the front of the server's view
        -- list
        createServerView :: Ptr WlrXdgSurface -> Ptr WlrXdgToplevel -> IO ()
        createServerView ptrWlrXdgSurface topLevel = mdo
          -- We use empty TVars to jam into some of the TinyWLView's fields
          falseBool <- atomically $ (newTVar False) :: IO (TVar Bool)
          -- Should we place all new surfaces at (0,0)?
          -- The C implementation doesn't specify this, so it's unclear how it knows where to place new surfaces (perhaps because C double types default to 0?).
          zeroOLC <- atomically $ newTVar (OutputLayoutCoordinates (0,0)) :: IO (TVar OutputLayoutCoordinates) 

          let tinyWLView = TinyWLView { _tvServer = tinyWLServer :: TinyWLServer
                                      , _tvXdgSurface              = ptrWlrXdgSurface                      :: Ptr WlrXdgSurface
                                      , _tvMap                     = (xdgSurfaceMap tinyWLView)            :: WlListener WlrXdgSurface
                                      , _tvUnmap                   = (xdgSurfaceUnmap tinyWLView)          :: WlListener WlrXdgSurface
                                      , _tvDestroy                 = (xdgSurfaceDestroy tinyWLView)        :: WlListener WlrXdgSurface
                                      , _tvRequestMove             = (xdgToplevelRequestMove tinyWLView )  :: WlListener MoveEvent
                                      , _tvRequestResize           = (xdgToplevelRequestResize tinyWLView) :: WlListener ResizeEvent
                                      , _tvMapped                  = falseBool                             :: TVar Bool
                                      , _tvOutputLayoutCoordinates = zeroOLC                               :: TVar OutputLayoutCoordinates
                                      , _tvListenerTokens          = tokens                                :: [ListenerToken]
                                      }

          -- Signals are divided into XdgSurfaceEvents and XdgTopLevelEvents
          let wlrXdgSurfaceEvents = getXdgSurfaceEvents ptrWlrXdgSurface
          let eventToplevelWlrXdgToplevelEvents = getXdgToplevelEvents topLevel

          -- We omit "timeout", "popup" signals
          let signalDestroy = xdgSurfaceEvtDestroy wlrXdgSurfaceEvents :: Ptr (WlSignal WlrXdgSurface)
          let signalMap = xdgSurfaceEvtMap wlrXdgSurfaceEvents :: Ptr (WlSignal WlrXdgSurface)
          let signalUnmap = xdgSurfaceEvtUnmap wlrXdgSurfaceEvents :: Ptr (WlSignal WlrXdgSurface)

          -- We omit "maximize", "fullscreen", "minimize", and "menu" signals
          let signalToplevelMove                = xdgToplevelEvtMove  eventToplevelWlrXdgToplevelEvents :: Ptr (WlSignal MoveEvent)
          let signalToplevelResize              = xdgToplevelEvtResize  eventToplevelWlrXdgToplevelEvents :: Ptr (WlSignal ResizeEvent)

          token1 <- addListener (tinyWLView ^. tvMap) signalMap                      -- xdg_surface event
          token2 <- addListener (tinyWLView ^. tvUnmap) signalUnmap                  -- xdg_surface event
          token3 <- addListener (tinyWLView ^. tvDestroy) signalDestroy              -- xdg_surface event
          token4 <- addListener (tinyWLView ^. tvRequestMove) signalToplevelMove     -- toplevel event
          token5 <- addListener (tinyWLView ^. tvRequestResize) signalToplevelResize -- toplevel event
          let tokens = [token1, token2, token3, token4, token5]

          -- We finally add/mutate the view to the head of our server's list
          -- (which is consistent with it being the "focused" view)
          serverViews <- atomically $ readTVar (tinyWLServer ^. tsViews)
          let serverViews' = [tinyWLView] ++ serverViews
          atomically $ writeTVar (tinyWLServer ^. tsViews) serverViews'