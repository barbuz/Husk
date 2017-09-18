import qualified Data.Map.Strict as Map
import Data.List ((\\), nub, inits)
import System.Environment (getArgs)
import Debug.Trace (trace)

type Node = (Int,[String],String)

main = do args <- getArgs
          dictionary <- readFile "dictionary.tsv"
          putStr $ unlines $ compressString (head args) $ buildDict dictionary
       where
         buildDict text = Map.fromList $ map splitTab $ lines text
         splitTab s | (first,tab:second) <- span (/='\t') s = (first,second)

compressString :: String -> Map.Map String String -> [String]
compressString s dictionary = astar [(length s, [""],map replaceNewlines s)] where

  replaceNewlines :: Char -> Char
  replaceNewlines '\n' = '¶'
  replaceNewlines c = c
  
  astar :: [Node] -> [String]
  astar ((_,encoded,[]):_) = encoded
  astar (node:nodes) = astar $ foldl insert nodes (expand node)
  
  insert :: [Node] -> Node -> [Node]
  insert [] n = [n]
  insert (h@(f,encoded,plain):nodes) n@(f2,encoded2,plain2) 
        | f<f2          = h: insert nodes n
        | f>f2          = n:h:nodes
        | plain==plain2 = (f,nub (encoded++encoded2),plain):nodes
        | otherwise     = h:n:nodes

  --estimated cost of a partial solution is the length of the part yet to be encoded plus 5 times the length of the encoded part
  --this is done because in the best case a string will be reduced to a fifth of its length
  expand :: Node -> [Node]
  expand (f,encoded,plain) = map extend ngrams where
    ngrams = filter (flip Map.member dictionary) $take 10 $ tail $ inits plain
    extend ngram = let Just code = Map.lookup ngram dictionary in 
                     (f+5*(length code)-length ngram, map (++code) encoded, plain\\ngram)