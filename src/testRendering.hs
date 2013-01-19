module Main where

import Data.Array.IO
import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU
import Graphics.Rendering.OpenGL.Raw.Core31
import qualified Marmoris.Field as F
import Marmoris.Rendering

import qualified Control.Exception as E

main = do
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 600 400
  initialWindowPosition $= Position 0 0
  initialize "testrendering" []

  window <- createWindow "Test"
  currentWindow $= Just window

  specialCallback $= Just (special window)

  -- f' <- F.loadField "../original/1.lvl"
  f' <- F.makeEmptyField
  f <- newListArray ((0,0), (4,4)) [1..] >>= \fd -> return (f' { F.fieldData = fd })
  stex <- defineStoneTextures f `E.catch` \(E.SomeException e) -> return []
  glGetError >>= print . show

  reshapeCallback $= Just (\s -> GL.viewport $= (GL.Position 0 0, s))

  displayCallback $= do
    -- GL.clearColor $= GL.Color4 0 0 0 1
    -- GL.clear [GL.ColorBuffer]
    -- GL.matrixMode $= GL.Projection
    -- GL.loadIdentity
    -- ortho2D 0 1 0 1  
    -- GL.matrixMode $= GL.Modelview 0
    -- GL.loadIdentity
    -- lookAt (Vertex3 0.5 0.5 (-0.5)) (Vertex3 0.5 0.5 0) (Vector3 0 1 0)
    initRendering
    renderField stex f >> GL.flush >> swapBuffers

  
  mainLoop


special :: Window -> SpecialKey -> Position -> IO ()
special w KeyDelete p = destroyWindow w
special _ _ _ = return ()