{-# LANGUAGE QuasiQuotes #-}

module DecompressString where

import qualified Data.Map.Lazy as Map
import FileQuoter

decompressString :: String -> String
decompressString s = go "" s where
  go prev (x:xs) | Just word <- Map.lookup (prev++[x]) dictionary = word ++ go "" xs
                 | otherwise                                      = go (prev++[x]) xs
  go _ [] = []
  
  
dictionary :: Map.Map String String
dictionary = Map.fromDistinctAscList $ read [litFile|dict.hs|] --This allows a quicker compilation than directly having the dictionary in this file