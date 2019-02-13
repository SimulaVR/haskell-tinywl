{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Control.Lens
import Data.Coerce

import System.Clock

import Graphics.Wayland.Server
import Graphics.Wayland.Internal.Server
import Graphics.Wayland.WlRoots.Compositor
import Graphics.Wayland.WlRoots.Output
import Graphics.Wayland.WlRoots.Surface
import Graphics.Wayland.WlRoots.Backend
import Graphics.Wayland.Signal
import Graphics.Wayland.WlRoots.Render
import Graphics.Wayland.WlRoots.OutputLayout
import Graphics.Wayland.WlRoots.Input
import Graphics.Wayland.WlRoots.Seat
import Graphics.Wayland.WlRoots.Cursor
import Graphics.Wayland.WlRoots.XCursorManager
import Graphics.Wayland.WlRoots.XdgShell
import Graphics.Wayland.WlRoots.Render
import Graphics.Wayland.WlRoots.DeviceManager
import Graphics.Wayland.WlRoots.Compositor
import Graphics.Wayland.WlRoots.Input.Pointer
import Graphics.Wayland.WlRoots.Util

import TinyWL

-- Turn off inline-C
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal
C.initializeTinyWLCtxAndIncludes

main :: IO ()
main = mdo setLogPrio Debug

           -- We omit implementing startup options

           displayServer <- throwIfNullPtr displayCreate
           ptrBackend    <- backendAutocreate displayServer -- automatically throws if NULL
           ptrRenderer   <- backendGetRenderer ptrBackend
           initWlDisplay displayServer ptrRenderer
           ptrWlrCompositor <- compositorCreate displayServer ptrRenderer
           ptrWlrDeviceManager <- managerCreate displayServer
           outputLayout <- createOutputLayout

           emptyViews       <- atomically $ (newTVar []) :: IO (TVar [TinyWLView])
           emptyKeyboards   <- atomically $ (newTVar []) :: IO (TVar [TinyWLKeyboard])
           cursorMode       <- atomically $ (newTVar TinyWLCursorPassthrough) :: IO (TVar (TinyWLCursorMode))
           emptyGrabbedView <- atomically $ (newTVar Nothing) :: IO (TVar (Maybe TinyWLView))
           emptyGrab        <- atomically $ (newTVar Nothing) :: IO (TVar (Maybe SurfaceLocalCoordinates))
           emptyGrabWidth   <- atomically $ (newTVar Nothing) :: IO (TVar (Maybe Int))
           emptyGrabHeight  <- atomically $ (newTVar Nothing) :: IO (TVar (Maybe Int))
           emptyResizeEdges <- atomically $ (newTVar Nothing) :: IO (TVar (Maybe Int))
           emptyOutputs     <- atomically $ (newTVar []) :: IO (TVar [TinyWLOutput])

           let signalBackendNewOutput = backendEvtOutput    (backendGetSignals ptrBackend)
           token1 <- addListener (tinyWLServer ^. tsNewOutput            ) signalBackendNewOutput

           -- BEGIN SHELL
           -- xdgShell <- xdgShellCreate (getListenerFunc (serverNewXdgSurface tinyWLServer)) displayServer -- This automatically adds the serverNewXdgSurface listener
           let displayServer' = toInlineC displayServer
           xdgShell' <- [C.exp| struct wlr_xdg_shell * { wlr_xdg_shell_create($(struct wl_display * displayServer'))} |]
           let xdgShell = toC2HS xdgShell'
           xdgShellSignal' <- [C.block| struct wl_signal * {struct wl_signal * signal_ptr;
                                                         signal_ptr = &$(struct wlr_xdg_shell * xdgShell')->events.new_surface;
                                                         return signal_ptr;} |]
           let xdgShellSignal = (toC2HS xdgShellSignal') :: Ptr (WlSignal WlrXdgSurface)
           token2 <- addListener (serverNewXdgSurface tinyWLServer) xdgShellSignal
           -- END SHELL

           ptrCursor <- createCursor
           attachOutputLayout ptrCursor outputLayout

           xCursorManager <- xCursorManagerCreate "" 24 -- Does empty string work?
           xCursorLoad xCursorManager 1
           let signalCursorMotion     = cursorMotion        (cursorGetEvents ptrCursor)
           let signalCursorMotionAbs  = cursorMotionAbs     (cursorGetEvents ptrCursor)
           let signalCursorButton     = cursorButton        (cursorGetEvents ptrCursor)
           let signalCursorAxis       = cursorAxis          (cursorGetEvents ptrCursor)
           token3 <- addListener (tinyWLServer ^. tsCursorMotion         ) signalCursorMotion
           token4 <- addListener (tinyWLServer ^. tsCursorMotionAbsolute ) signalCursorMotionAbs
           token5 <- addListener (tinyWLServer ^. tsCursorButton         ) signalCursorButton
           token6 <- addListener (tinyWLServer ^. tsCursorAxis           ) signalCursorAxis

           let signalBackendNewInput  = backendEvtInput     (backendGetSignals ptrBackend)
           token7 <- addListener (tinyWLServer ^. tsNewInput      ) signalBackendNewInput

           seat <- createSeat displayServer "seat0"
           let signalRequestSetCursor = seatSignalSetCursor (seatGetSignals seat)
           token8 <- addListener (tinyWLServer ^. tsRequestCursor ) signalRequestSetCursor

           let tinyWLServer = TinyWLServer { _tsBackend              = ptrBackend                                :: Ptr Backend
                                           , _tsRenderer             = ptrRenderer                               :: Ptr Renderer
                                           , _tsXdgShell             = xdgShell                                  :: Ptr WlrXdgShell
                                           , _tsNewXdgSurface        = (serverNewXdgSurface tinyWLServer)        :: WlListener WlrXdgSurface
                                           , _tsViews                = emptyViews                                :: TVar [TinyWLView]
                                           , _tsCursor               = ptrCursor                                 :: Ptr WlrCursor
                                           , _tsCursorManager        = xCursorManager                            :: Ptr WlrXCursorManager
                                           , _tsCursorMotion         = (serverCursorMotion tinyWLServer)         :: WlListener WlrEventPointerMotion
                                           , _tsCursorMotionAbsolute = (serverCursorMotionAbsolute tinyWLServer) :: WlListener WlrEventPointerAbsMotion
                                           , _tsCursorButton         = (serverCursorButton tinyWLServer)         :: WlListener WlrEventPointerButton
                                           , _tsCursorAxis           = (serverCursorAxis tinyWLServer)           :: WlListener WlrEventPointerAxis
                                           , _tsSeat                 = seat                                      :: Ptr WlrSeat
                                           , _tsNewInput             = (serverNewInput tinyWLServer)             :: WlListener InputDevice
                                           , _tsRequestCursor        = (seatRequestCursor tinyWLServer)          :: WlListener SetCursorEvent
                                           , _tsKeyboards            = emptyKeyboards                            :: TVar [TinyWLKeyboard]
                                           , _tsCursorMode           = cursorMode
                                           , _tsGrabbedView          = emptyGrabbedView                          :: TVar (Maybe TinyWLView)
                                           , _tsGrab                 = emptyGrab                                 :: TVar (Maybe SurfaceLocalCoordinates)
                                           , _tsGrabWidth            = emptyGrabWidth                            :: TVar (Maybe Int)
                                           , _tsGrabHeight           = emptyGrabHeight                           :: TVar (Maybe Int)
                                           , _tsResizeEdges          = emptyResizeEdges                          :: TVar (Maybe Int)
                                           , _tsOutputLayout         = outputLayout                              :: Ptr WlrOutputLayout
                                           , _tsOutputs              = emptyOutputs                              :: TVar [TinyWLOutput]
                                           , _tsNewOutput            = (serverNewOutput tinyWLServer)            :: WlListener WlrOutput
                                           , _tsListenerTokens       = tokens                                    :: [ListenerToken]
                                           }

           let tokens = [token1, token2, token3, token4, token5, token6, token7, token8]

           displayAddSocket displayServer Nothing -- Does this work as wl_display_add_socket_auto?
                                                 -- NOTE: If we were proper here we'd check if ther ewas a socket actually created before proceeding

           backendStart ptrBackend -- Doesn't destroy the wl_display; potential memory leak
                                   -- NOTE: If we were proper here we'd actually confirm the backend started before proceeding

           -- setenv("WAYLAND_DISPLAY", socket, true);
           -- wlr_log(WLR_INFO, "Running Wayland compositor on WAYLAND_DISPLAY=%s", socket);
           displayRun displayServer
           -- wl_display_destroy_clients(server.wl_display);
           displayDestroy displayServer
           mapM_ freeListenerToken tokens

           -- tinywl.c omits destroying almost all of the data structures we
           -- created above (i.e., the compositor, ..). Why?
           return ()

  where throwIfNullPtr f = throwErrnoIf
                           (\res -> ((coerce res :: Ptr ()) == nullPtr)) -- Throw error if the pointer returned by f is 0 (i.e., NULL)
                           "wl_* initialization failed."
                           f
        getListenerFunc :: WlListener a -> (Ptr a -> IO ())
        getListenerFunc (WlListener func) = func

-- | This launches the C implementation of tinywl (useful for test purposes).
tinyWLC :: IO ()
tinyWLC = do
  [C.block| int { tinywl();}|]
  return ()