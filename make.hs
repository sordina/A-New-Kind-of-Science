import System.Process
import Control.Monad
import System.Environment
import System.Directory

ghc ="ghc"
 
main = do
       args <- getArgs
       forM ["110","135","30","90","89"] $ \rule -> do
         let source_name = "rule_"++rule
         when (args==["clean"]) $
           forM_ [".o",".hi",""] $ \extension -> do
             let file_name = source_name ++ extension
             fileExists <- doesFileExist file_name
             when fileExists (removeFile file_name)
         system $ ghc++" -O --make "++source_name++".hs"
       return ()

