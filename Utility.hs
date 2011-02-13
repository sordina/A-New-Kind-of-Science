module Utility (fromIndex, runFromIndex, fromString, runFromString, run)
where

import OneDSimple
import Control.Monad (replicateM)

runFromString :: String -> IO ()
runFromString s = run s (fromString s) 300 700 Center

fromString :: String -> Successor
fromString [a,b,c,d,e,f,g,h] = x
  where
    x (False, False, False) = y a
    x (False, False, True)  = y b
    x (False, True,  False) = y c
    x (False, True,  True)  = y d
    x (True,  False, False) = y e
    x (True,  False, True)  = y f
    x (True,  True,  False) = y g
    x (True,  True,  True)  = y h

fromString _ = error "Requires an Octuplet"

y ' ' = False
y 'f' = False
y 't' = True
y _   = error "String can only contain ' ' (false), 'f' (false), and 't' (true) characters"


fromIndex :: Int -> Successor
fromIndex = fromString . ruleToString

ruleToString = (replicateM 8 "ft" !!)

runFromIndex :: Int -> IO ()
runFromIndex n
  | n < 0     = error "Index must be greater than or equal to 0"
  | n > 255   = error "Index must be less than 255"
  | otherwise = run s (fromIndex n) 300 700 Center
  where
    s = "Index " ++ show n
