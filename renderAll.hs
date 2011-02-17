import OneDSimple
import Utility
import Control.Monad

main = mapM_ render indexedAutomata

render (i,a) = do
  print i
  writeFile name image
    where
      name  = "images/" ++ (show i) ++ ".pbm"
      rows  = take 700 a
      image = unlines ["P1", "301 700", unlines . map (unwords . map toString) $ rows]

toString True  = "1"
toString False = "0"

automata = map (flip mkAutomata initial . fromString) . replicateM 8 $ "ft"

indexedAutomata = zip [0..] automata

initial = mkRow Center 300
