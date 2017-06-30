module DecompressString where

import qualified Data.Map.Lazy as Map
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getExecutablePath)
import System.FilePath (replaceFileName)

decompressString :: String -> String
decompressString s = go "" s where
  go prev (x:xs) | Just word <- Map.lookup (prev++[x]) dictionary = word ++ go "" xs
                 | otherwise                                      = go (prev++[x]) xs
  go _ [] = []
  
  
dictionary :: Map.Map String String
--We need to read it at runtime, otherwise compilation is too slow and memory-hungry
dictionary = Map.fromDistinctAscList $ read listdict where
  listdict = unsafePerformIO $ getDict
  getDict = do
              path <- getExecutablePath
              readFile $ replaceFileName path "dict.hs"