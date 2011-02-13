import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

main = do
  pixels <- newIORef automata
  getArgsAndInitialize
  createWindow "1D Finite Automata"
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= Size 400 400
  displayCallback    $= renderLoop pixels
  idleCallback       $= Just (renderLoop pixels)
  mainLoop

renderLoop ioPixels = do
  pixels <- cyclePixels ioPixels -- Main pixel-cycling logic
  flushBefore
  renderRows pixels -- Main drawing logic
  flushAfter

renderRows rows = renderPrimitive Points (mapM_ renderRow $ zip [200,199..] rows)

renderRow (y,row) = mapM_ renderCell $ zip [-200,-199..] row
  where
     renderCell (x,c) = mkColor c >> v (x/200) (y/200)

mkColor True  = color $ Color3 0 0 (0::GLdouble)
mkColor False = color $ Color3 1 0 (0::GLdouble)

v :: GLdouble -> GLdouble -> IO ()
v a b = vertex $ Vertex2 a b

cyclePixels ioPixels = do
  allPixels <- readIORef ioPixels

  let
    thisSet = take 400 allPixels
    nextSet = tail allPixels -- Update 1 row at a time

  writeIORef ioPixels nextSet
  return thisSet

-- Automata

triplify l = zip3
  (tail (cycle l))
  l
  (False : l)

row1 = replicate 400 False

automata = iterate (map progression . triplify) row1

-- Cell successor function

progression (False,False,False) = True
progression (False,False,True)  = False
progression (False,True, False) = False
progression (False,True, True)  = True
progression (True, False,False) = False
progression (True, False,True)  = False
progression (True, True, False) = False
progression (True, True, True)  = True

-- Convenience Stuff

flushBefore = do
  clear [ColorBuffer]
  pointSize $= 1

flushAfter = do
  flush
  swapBuffers
