module OneDSimple (run, mkAutomata, Alignment(..))
where

import Prelude hiding (Left, Right) -- Left, Right collide with allignments
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

-- Data Types
data Alignment = Left | Center | Right

-- Main entry function
run :: ((Bool,Bool,Bool) -> Bool) -> Int -> Int -> Alignment -> IO ()
run f width height alignment = do

  -- Set up references
  pixels <- newIORef (mkAutomata f (mkRow alignment width))

  -- Initialize GL state
  getArgsAndInitialize
  initialWindowSize  $= Size (fromIntegral width) (fromIntegral height)

  -- Create window
  createWindow "1D Finite Automata"

  -- Initialize window state
  let renderer = renderLoop width height pixels

  displayCallback    $= renderer
  idleCallback       $= Just renderer
  initialDisplayMode $= [DoubleBuffered]

  -- Begin OpenGL loop
  mainLoop

renderLoop :: Int -> Int -> IORef [[Bool]] -> IO ()
renderLoop width height ioPixels = do
  pixels <- cyclePixels height ioPixels -- Main pixel-cycling logic
  flushBefore
  renderRows pixels -- Main drawing logic
  flushAfter

  where
    w = fi width
    h = fi height

    renderRows :: [[Bool]] -> IO ()
    renderRows rows = renderPrimitive Points $ mapM_ renderRow indexedRows
      where
        indexedRows :: [(Int,[Bool])]
        indexedRows = zip (down (height`div`2)) rows

    renderRow :: (Int, [Bool]) -> IO ()
    renderRow (y,row) = mapM_ renderCell indexedCells
      where
        indexedCells :: [(Int,Bool)]
        indexedCells = zip (up (-width`div`2)) row

        renderCell :: (Int,Bool) -> IO ()
        renderCell (x,c) = mkColor c >> v (2*fi x/w) (2*fi y/h)

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

triplify l = zip3      (tail (cycle l))       l       (False : l)

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

fi = fromIntegral

up   x = [x,x+1..]
down x = [x,x-1..]
