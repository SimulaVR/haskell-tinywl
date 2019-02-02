{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TinyWL where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Data.Maybe

import           Foreign
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
                                 , _tsKeyboards :: TVar [TinyWLKeyboard]
                                 , _tsCursorMode :: TinyWLCursorMode
                                 , _tsGrabbedView :: TinyWLView
                                 , _tsGrab :: TVar SurfaceLocalCoordinates -- The surface-local point at which a surface is grabbed.
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
                             , _tvOutputLayoutCoordinates :: TVar OutputLayoutCoordinates -- Denotes top-left corner of view in OutputLayoutCoordinates
                             }

data TinyWLKeyboard = TinyWLKeyboard { _tkServer :: TinyWLServer
                                     , _tkDevice :: Ptr InputDevice
                                     , _tkModifiers :: WlListener ()
                                     , _tkKey :: WlListener EventKey
                                     }

data RenderData = RenderData { _rdOutput :: Ptr WlrOutput
                             , _rdrRenderer :: Ptr Renderer
                             , _rdView :: TinyWLView
                             , _rdWhen :: TimeSpec
                             }

-- | Helper function to convert Haskell timestamps to milliseconds (which wlroots expects) for C marshalling.
toMsec32 :: TimeSpec -> IO (Word32)
toMsec32 timeSpec = do
  let msec = fromIntegral $ toNanoSecs timeSpec `div` 1000000
  return msec

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
  let tinyWLKeyboard = TinyWLKeyboard { _tkServer    = server                                   :: TinyWLServer
                                      , _tkDevice    = device                                   :: Ptr InputDevice
                                      , _tkModifiers = (keyboardHandleModifiers tinyWLKeyboard) :: WlListener ()
                                      , _tkKey       = (keyboardHandleKey       tinyWLKeyboard) :: WlListener EventKey
                                      }

  deviceType <- inputDeviceType device
  maybeXkbContext <- newContext defaultFlags

  case (deviceType, maybeXkbContext) of
       ((DeviceKeyboard ptrWlrKeyboard), (Just xkbContext)) -> handleKeyboard tinyWLKeyboard ptrWlrKeyboard xkbContext
       _ -> putStrLn "Failed to get keyboard!"
  where handleKeyboard :: TinyWLKeyboard -> Ptr WlrKeyboard -> Context -> IO ()
        handleKeyboard tinyWLKeyboard deviceKeyboard context = do
          maybeKeymap <- newKeymapFromNames context noPrefs
          case maybeKeymap of
               Nothing       -> putStrLn "Failed to get keymap!"
               (Just keymap) -> do setKeymapAndRepeatInfo deviceKeyboard keymap
                                   handleSignals tinyWLKeyboard deviceKeyboard
                                   makeKeyboardActiveHead tinyWLKeyboard

        setKeymapAndRepeatInfo :: Ptr WlrKeyboard -> Keymap -> IO ()
        setKeymapAndRepeatInfo keyboard keymap = do
          withKeymap keymap (\keyMapC -> setKeymap keyboard keyMapC)
          let keyboard' = toInlineC keyboard
          [C.exp| void { wlr_keyboard_set_repeat_info($(struct wlr_keyboard * keyboard'), 25, 600) }|] -- hsroots doesn't provide this call.

        handleSignals :: TinyWLKeyboard -> Ptr WlrKeyboard -> IO ()
        handleSignals tinyWLKeyboard keyboard = do
          let keyboardSignals = getKeySignals keyboard
          let keySignalKey' = (keySignalKey keyboardSignals)
          let keySignalModifiers' = (keySignalModifiers keyboardSignals)
          addListener (_tkModifiers tinyWLKeyboard) keySignalModifiers' -- a = TinyWLKeyboard
          addListener (_tkKey tinyWLKeyboard)       keySignalKey' -- a = EventKey
          return ()

        makeKeyboardActiveHead :: TinyWLKeyboard -> IO ()
        makeKeyboardActiveHead tinyWLKeyboard = do
          seatSetKeyboard (_tsSeat server) (_tkDevice tinyWLKeyboard)
          keyboardList <- atomically $ readTVar (_tsKeyboards server)
          atomically $ writeTVar (_tsKeyboards server) ([tinyWLKeyboard] ++ keyboardList)

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
     let mouseCapability = SeatCapability (deviceTypeToInt (DevicePointer undefined))
     let keyboardCapability = SeatCapability (deviceTypeToInt (DeviceKeyboard undefined))
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
   grabbedViewTopLeft@(OutputLayoutCoordinates (grabbedViewLX, grabbedViewLY)) <- atomically $ readTVar (_tvOutputLayoutCoordinates (_tsGrabbedView tinyWLServer))
   surfaceGrabPoint@(SurfaceLocalCoordinates (surfaceGrabPointSX, surfaceGrabPointSY)) <- atomically $ readTVar (_tsGrab tinyWLServer)

   let cursor = (_tsCursor tinyWLServer)
   cursorLX <- getCursorX cursor
   cursorLY <- getCursorY cursor
   let cursorPosition = OutputLayoutCoordinates (cursorLX, cursorLY)

   let newGrabbedViewTopLeft = OutputLayoutCoordinates (cursorLX - surfaceGrabPointSX, cursorLY - surfaceGrabPointSY)
   atomically $ writeTVar (_tvOutputLayoutCoordinates (_tsGrabbedView tinyWLServer)) newGrabbedViewTopLeft

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
  let cursorMode = (_tsCursorMode tinyWLServer)
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