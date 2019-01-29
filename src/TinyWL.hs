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

import Text.XkbCommon.Keysym

import Graphics.Wayland.Server
import Graphics.Wayland.Internal.Server
import Graphics.Wayland.WlRoots.Compositor
import Graphics.Wayland.WlRoots.Output
import Graphics.Wayland.WlRoots.Surface
import Graphics.Wayland.WlRoots.Backend
import Graphics.Wayland.Signal
import Graphics.Wayland.WlRoots.Render
import Graphics.Wayland.WlRoots.Render.Color
import Graphics.Wayland.WlRoots.OutputLayout
import Graphics.Wayland.WlRoots.Input
import Graphics.Wayland.WlRoots.Seat
import Graphics.Wayland.WlRoots.Cursor
import Graphics.Wayland.WlRoots.XCursorManager
import Graphics.Wayland.WlRoots.XdgShell
import Graphics.Wayland.WlRoots.Input.Keyboard

import           System.Clock


-- Turn off inline-C debugging
-- C.initializeTinyWLCtxAndIncludes

data TinyWLCursorMode = TinyWLCursorPassthrough | TinyWLCursorMove |  TinyWLCursorResize

data TinyWLServer = TinyWLServer { _tsBackend :: Ptr Backend
                                 , _tsRenderer :: Ptr Renderer
                                 , _tsXdgShell :: Ptr WlrXdgShell -- [X] Needs FFI implementation
                                 , _tsNewXdgSurface :: WlListener ()
                                 , _tsViews :: [TinyWLView]
                                 , _tsCursor :: Ptr WlrCursor -- [X] Needs FFI implementation
                                 , _tsCursorManager :: Ptr WlrXCursorManager -- [X] Needs FFI implementation
                                 , _tsCursorMotion :: WlListener ()
                                 , _tsCursorMotionAbsolute :: WlListener ()
                                 , _tsCursorButton :: WlListener ()
                                 , _tsCursorAxis :: WlListener ()
                                 , _tsSeat :: Ptr WlrSeat -- [X] FFI
                                 , _tsNewInput :: WlListener ()
                                 , _tsRequestCursor :: WlListener ()
                                 , _tsKeyboards :: [Ptr WlrKeyboard] -- [X] FFI
                                 , _tsCursorMode :: TinyWLCursorMode
                                 , _tsGrabbedView :: TinyWLView
                                 , _tsGrabX :: Double
                                 , _tsGrabY :: Double
                                 , _tsGrabWidth :: Int
                                 , _tsResizeEdges :: Int -- Positive number type in Haskell?
                                 , _tsOutputLayout :: Ptr WlrOutputLayout -- [X] FFI
                                 , _tsOutputs :: [TinyWLOutput]
                                 , _tsNewOutput :: WlListener ()
                                 }

data TinyWLOutput = TinyWLOutput { _toServer :: TinyWLServer
                                , _toWlrOutput :: Ptr WlrOutput -- [X] FFI
                                , _toFrame :: WlListener ()
                                }

data TinyWLView = TinyWLView { _tvServer :: TinyWLServer
                             , _tvXdgSurface :: Ptr WlrXdgSurface -- [X] FFI
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
                                     , _tkDevice :: Ptr InputDevice -- [X] FFI
                                     , _tkModifiers :: WlListener ()
                                     , _tkKey :: WlListener ()
                                     }

data RenderData = RenderData { _rdOutput :: Ptr WlrOutput
                             , _rdrRenderer :: Ptr Renderer
                             , _rdView :: TinyWLView
                             , _rdWhen :: TimeSpec
                             }

focusView :: TinyWLView -> Ptr WlrSurface
focusView = undefined

keyboardHandleModifiers :: WlListener TinyWLKeyboard
keyboardHandleModifiers = undefined

handleKeybinding :: TinyWLServer -> Ptr ()
handleKeybinding = undefined
-- Return type should be "Keysym" or "CKeysym" but is part of XkbCommon.InternalTypes i.e.:
-- newtype CKeysym = CKeysym {unCKeysym :: #{type xkb_keysym_t}} deriving (Show, Eq)

keyboardHandleKey :: WlListener TinyWLKeyboard
keyboardHandleKey = undefined

serverNewKeyboard :: TinyWLServer -> Ptr InputDevice
serverNewKeyboard = undefined

serverNewPointer :: TinyWLServer -> Ptr InputDevice
serverNewPointer = undefined

serverNewInput :: WlListener TinyWLServer
serverNewInput = undefined

seatRequestCursor :: WlListener TinyWLServer
seatRequestCursor = undefined

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

serverCursorMotion :: WlListener ()
serverCursorMotion = undefined

serverCursorMotionAbsolute :: WlListener ()
serverCursorMotionAbsolute = undefined

serverCursorButton :: WlListener ()
serverCursorButton = undefined

serverCursorAxis :: WlListener ()
serverCursorAxis = undefined

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

xdgToplevelRequestMove :: WlListener ()
xdgToplevelRequestMove = undefined

xdgToplevelRequestResize :: WlListener ()
xdgToplevelRequestResize = undefined

serverNewXdgSurface :: WlListener ()
serverNewXdgSurface = undefined

