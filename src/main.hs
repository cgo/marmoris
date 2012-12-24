module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

reshape :: Size -> IO ()
reshape (Size w h) =
  viewport $= (Position 0 0, Size w h) >>
  putStrLn (show w ++ " " ++ show h)

mouse :: MouseButton -> KeyState -> Position -> IO ()
mouse LeftButton Down (Position x y) = putStrLn $ "Pressed at " ++ show x ++ show y
mouse _ _ _ = return ()

keyboard :: Char -> Position -> IO ()
keyboard c (Position x y) = case c of
  'q' -> leaveMainLoop
  otherwise  -> return ()

display :: IO ()
display = do
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 1 0 1
  matrixMode $= Modelview 0
  loadIdentity
  -- lookAt (Vertex3 0.5 0.5 (-0.5)) (Vertex3 0.5 0.5 0) (Vector3 0 1 0)

  clearColor $= Color4 0 0 0 1
  clear [ColorBuffer]

  renderPrimitive Quads $ do
    color $ Color3 0.5 1 (1::GLfloat)
    vertex $ Vertex3 0.25 0.25 (0::GLfloat)
    vertex $ Vertex3 0.75 0.25 (0::GLfloat)
    vertex $ Vertex3 0.75 0.75 (0::GLfloat)
    vertex $ Vertex3 0.25 0.75 (0::GLfloat)

  flush

main = do
  initialDisplayMode $= [RGBMode, SingleBuffered]
  initialize "MyProgram" []
  
  window <- createWindow "My window"
  currentWindow $= Just window
  
  reshapeCallback $= Just reshape
  mouseCallback $= Just mouse
  keyboardCallback $= Just keyboard
  displayCallback $= display
  
  mainLoop

  return ()