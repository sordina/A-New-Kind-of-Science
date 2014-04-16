module OneDSimple (run, mkAutomata, mkRow, Successor, Alignment(..))
where

import Prelude hiding (Left, Right) -- Left, Right collide with allignments
import Graphics.UI.GLUT
import Data.IORef

-- Data Types
data Alignment = Left | Center | Right

type Successor = (Bool,Bool,Bool) -> Bool

-- Main entry function
run :: String -> Successor -> Int -> Int -> Alignment -> IO ()
run name f width height alignment = do

  -- Set up references
  pixels <- newIORef (mkAutomata f (mkRow alignment width))

  -- Initialize GL state
  getArgsAndInitialize
  initialWindowSize  $= Size (fromIntegral width) (fromIntegral height)
  initialDisplayMode $= [DoubleBuffered]

  -- Create window
  createWindow $ name ++ " -- Finite Automata"

  -- pointSmooth $= Enabled
  pointSize $= 1

  -- Define callbacks
  let
    renderer = renderLoop width height pixels

    pause _ Down _ _ = keyboardMouseCallback $= Just resume >> idleCallback $= Nothing
    pause _ _    _ _ = return ()

    resume _ Down _ _ = keyboardMouseCallback $= Just pause >> idleCallback $= Just renderer
    resume _ _    _ _ = return ()

  -- Initialize window state
  displayCallback       $= renderer
  idleCallback          $= Just renderer
  keyboardMouseCallback $= Just pause

  -- Begin OpenGL loop
  mainLoop

renderLoop :: Int -> Int -> IORef [[Bool]] -> IO ()
renderLoop width height ioPixels = do
  pixels <- cyclePixels height ioPixels -- Main pixel-cycling logic
  -- flushBefore
  renderRows pixels -- Main drawing logic
  flushAfter

  where
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
        renderCell (x,c) = mkColor c >> v (2*fi x/fi width) (2*fi y/fi height)

mkColor True  = colorTrue
mkColor False = colorFalse

colorTrue  = color $ Color3 0.7  0.8  (0.9::GLdouble)
colorFalse = color $ Color3 0.04 0.05 (0.06::GLdouble)

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
mkRow Center width = replicate w False ++ [True] ++ replicate w False where w = width `div` 2
mkRow Right  width = replicate (width-1) False ++ [True]

mkAutomata f = iterate (map f . triplify)

-- Convenience Stuff

--flushBefore = do --clear [ColorBuffer] -- pointSize $= 1

flushAfter = do
  swapBuffers
  -- flush

fi = fromIntegral

up   x = [x,x+1..]
down x = [x,x-1..]
