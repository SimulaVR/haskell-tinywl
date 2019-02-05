{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           Foreign
import           Foreign.C.Types
import           Foreign.C.Error
import           Control.Lens
-- import qualified Language.C.Inline as C
-- import           Debug.C as C
-- import           Debug.Marshal
import           Data.Coerce

import           System.Clock


-- TODO: Remove unneeded hsroot modules
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

import           TinyWL

-- Turn off inline-C
-- C.initializeTinyWLCtxAndIncludes

main :: IO ()
main = do setLogPrio Debug -- Silent, Error, Info, Debug
          -- We omit implementing startup options
          displayServer <- throwIfNullPtr displayCreate
          eventLoop     <- throwIfNullPtr (displayGetEventLoop displayServer)
          ptrBackend    <- backendAutocreate displayServer -- automatically throws if NULL
          ptrRenderer   <- backendGetRenderer ptrBackend
          initWlDisplay displayServer ptrRenderer 
          ptrWlrCompositor <- compositorCreate displayServer ptrRenderer
          ptrWlrDeviceManager <- managerCreate displayServer
          outputLayout <- createOutputLayout

          emptyViews <- atomically $ (newTVar []) :: IO (TVar [TinyWLView])
          emptyKeyboards <- atomically $ (newTVar []) :: IO (TVar [TinyWLKeyboard])
          cursorMode <- atomically $ (newTVar TinyWLCursorPassthrough) :: IO (TVar (TinyWLCursorMode))
          emptyGrabbedView <- atomically $ (newTVar undefined) :: IO (TVar (TinyWLView))
          emptyGrab <- atomically $ (newTVar undefined) :: IO (TVar (SurfaceLocalCoordinates))
          emptyGrabWidth <- atomically $ (newTVar undefined) :: IO (TVar (Int))
          emptyGrabHeight <- atomically $ (newTVar undefined) :: IO (TVar (Int))
          emptyResizeEdges <- atomically $ (newTVar undefined) :: IO (TVar (Int))
          emptyOutputs <- atomically $ (newTVar []) :: IO (TVar [TinyWLOutput])

          ptrCursor <- createCursor
          attachOutputLayout ptrCursor outputLayout

          xCursorManager <- xCursorManagerCreate "" 24 -- Does empty string work?
          xCursorLoad xCursorManager 1

          seat <- createSeat displayServer "seat0"
          
          rec xdgShell <- xdgShellCreate (getListenerFunc (serverNewXdgSurface tinyWLServer)) displayServer -- This automatically adds the serverNewXdgSurface listener
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
                                              , _tsGrabbedView          = emptyGrabbedView                          :: TVar TinyWLView
                                              , _tsGrab                 = emptyGrab                                 :: TVar SurfaceLocalCoordinates -- The surface-local point at which a surface is grabbed.
                                              , _tsGrabWidth            = emptyGrabWidth                            :: TVar Int
                                              , _tsGrabHeight           = emptyGrabHeight                           :: TVar Int
                                              , _tsResizeEdges          = emptyResizeEdges                          :: TVar Int
                                              , _tsOutputLayout         = outputLayout                              :: Ptr WlrOutputLayout
                                              , _tsOutputs              = emptyOutputs                              :: TVar [TinyWLOutput]
                                              , _tsNewOutput            = (serverNewOutput tinyWLServer)            :: WlListener WlrOutput
                                              }

          let signalBackendNewOutput = backendEvtOutput    (backendGetSignals ptrBackend)
          let signalBackendNewInput  = backendEvtInput     (backendGetSignals ptrBackend)
          let signalCursorMotion     = cursorMotion        (cursorGetEvents ptrCursor)
          let signalCursorMotionAbs  = cursorMotionAbs     (cursorGetEvents ptrCursor)
          let signalCursorButton     = cursorButton        (cursorGetEvents ptrCursor)
          let signalCursorAxis       = cursorAxis          (cursorGetEvents ptrCursor)
          let signalRequestSetCursor = seatSignalSetCursor (seatGetSignals seat)

          addListener (tinyWLServer ^. tsNewOutput            ) signalBackendNewOutput
          addListener (tinyWLServer ^. tsNewInput             ) signalBackendNewInput
          addListener (tinyWLServer ^. tsCursorMotion         ) signalCursorMotion
          addListener (tinyWLServer ^. tsCursorMotionAbsolute ) signalCursorMotionAbs
          addListener (tinyWLServer ^. tsCursorButton         ) signalCursorButton
          addListener (tinyWLServer ^. tsCursorAxis           ) signalCursorAxis
          addListener (tinyWLServer ^. tsRequestCursor        ) signalRequestSetCursor

          displayAddSocket displayServer Nothing -- Does this work as wl_display_add_socket_auto?
                                                 -- NOTE: If we were proper here we'd check if ther ewas a socket actually created before proceeding

          backendStart ptrBackend -- Doesn't destroy the wl_display; potential memory leak
                                  -- NOTE: If we were proper here we'd actually confirm the backend started before proceeding

          -- setenv("WAYLAND_DISPLAY", socket, true);
          -- wlr_log(WLR_INFO, "Running Wayland compositor on WAYLAND_DISPLAY=%s", socket);
          displayRun displayServer
          displayDestroy displayServer
          -- wl_display_destroy_clients(server.wl_display);

          -- tinywl.c omits destroying almost all of the data structures we
          -- created above (i.e., the compositor, ..). Why?

  where throwIfNullPtr f = throwErrnoIf
                           (\res -> ((coerce res :: Ptr ()) == nullPtr)) -- Throw error if the pointer returned by f is 0 (i.e., NULL)
                           "wl_* initialization failed."
                           f
        getListenerFunc :: WlListener a -> (Ptr a -> IO ())
        getListenerFunc (WlListener func) = func