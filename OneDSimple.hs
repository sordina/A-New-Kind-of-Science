module OneDSimple (run, mkAutomata, Alignment(..))
where

import Prelude hiding (Left, Right)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

fi = fromIntegral

run :: ((Bool,Bool,Bool) -> Bool) -> Int -> Int -> Alignment -> IO ()
run f width height alignment = do

  -- Set up references
  pixels <- newIORef (mkAutomata f (mkRow alignment width))

  -- Initialize GL state
  getArgsAndInitialize
  initialWindowSize  $= Size (fi width) (fi height)

  -- Create window
  createWindow "1D Finite Automata"

  -- Initialize window state
  initialDisplayMode $= [DoubleBuffered]
  displayCallback    $= renderLoop height pixels
  idleCallback       $= Just (renderLoop height pixels)

  -- Begin procedings
  mainLoop

renderLoop height ioPixels = do
  pixels <- cyclePixels height ioPixels -- Main pixel-cycling logic
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

cyclePixels height ioPixels = do
  allPixels <- readIORef ioPixels

  let
    thisSet = take height allPixels
    nextSet = tail allPixels -- Update 1 row at a time

  writeIORef ioPixels nextSet
  return thisSet

-- Automata

triplify l = zip3
  (tail (cycle l))
  l
  (False : l)

data Alignment = Left | Center | Right

mkRow Left   width = True : replicate (width-1) False
mkRow Center width = replicate w False ++ [True] ++ replicate w False where w = (width`div`2)
mkRow Right  width = replicate (width-1) False ++ [True]

mkAutomata f = iterate (map f . triplify)

-- Convenience Stuff

flushBefore = do
  clear [ColorBuffer]
  pointSize $= 1

flushAfter = do
  flush
  swapBuffers
