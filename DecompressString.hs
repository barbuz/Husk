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
dictionary = Map.fromDistinctAscList $ map splitTabs $ lines dict where
    splitTabs s | (first,tab:second) <- span (/='\t') s = (first,second)
    dict = unsafePerformIO $ getDict
    getDict = do
               path <- getExecutablePath
               readFile $ replaceFileName path "revdictionary.tsv"
