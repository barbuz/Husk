import qualified Data.Map.Strict as Map
import Data.List (inits)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn,stderr)
--import Debug.Trace (trace)

type Node = (Int,[String])

consoleOpts :: [OptDescr Bool]
consoleOpts = [Option ['1'] [] (NoArg $ True) "Return only a single result"]

main = do args <- getArgs
          let parsedArgs = getOpt Permute consoleOpts args
          
          case parsedArgs of
            (opt,plainText:[],[]) -> do
              putStrLn $ "Original length: "++show (length plainText)++" bytes."
              dictionary <- readFile "dictionary.tsv"
              let results = compressString (elem True opt) plainText $ buildDict dictionary
              putStrLn $ "Compressed length: "++show (length $ head results)++" bytes."
              putStr $ unlines $ map quote results
            (_,_,errors) -> hPutStrLn stderr $ unlines errors ++ usageInfo "Usage: compressString [OPT] string" consoleOpts
       where
         buildDict text = Map.fromDistinctAscList $ map splitTab $ lines text
         splitTab s | (first,tab:second) <- span (/='\t') s = (first,second)
         quote s = '¨':s++"¨"

replaceNewlines :: Char -> Char
replaceNewlines '\n' = '¶'
replaceNewlines c = c

compressString :: Bool -> String -> Map.Map String String -> [String]
compressString opt s dictionary = go [(0, [""])] (map replaceNewlines s) where
  
  go :: [Node] -> String -> [String]
  go ((_,encoded):_) []      = encoded
  go (node:nodes) plain  = go (chooseBest nodes (extend node (encodeStart plain))) $ tail plain
  
  encodeStart :: String -> [Node]
  encodeStart s = map buildNode $ take maxDictWordLen $ tail $ inits s where
    maxDictWordLen = 10
    buildNode ngram | Just code <- Map.lookup ngram dictionary = (length code,[code])
                    | otherwise                                = (0,[])

  extend :: Node -> [Node] -> [Node]
  extend node nodes = map (addNode node) nodes where
    addNode _ (0,[]) = (0,[])
    addNode (l1,previous) (l2,[current]) = (l1+l2,map (++current) previous)
  
  chooseBest :: [Node] -> [Node] -> [Node]
  chooseBest  as             []            = as
  chooseBest  []             bs            = bs
  chooseBest (a:as)         (b@(_,[]):bs)  = a:chooseBest as bs        
  chooseBest (a@(_,[]):as)  (b:bs)         = b:chooseBest as bs   
  chooseBest (a@(l1,w1):as) (b@(l2,w2):bs) | l1 < l2             = a:chooseBest as bs
                                           | l1 ==l2 && not opt  = (l1,w1++w2):chooseBest as bs
                                           | otherwise           = b:chooseBest as bs
