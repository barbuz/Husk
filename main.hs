
-- Main program

import Expr
import Infer
import Parser
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.List (find)

-- Wrapper for expression parser
parseProg :: String -> Either String [(Type, Exp Lit)]
parseProg s = inferType <$> parseExpr s

-- Command line option flags
data Flag = InferType
          | InFile
          | OutFile String
          deriving (Eq)

isOutFile :: Flag -> Bool
isOutFile (OutFile _) = True
isOutFile _ = False

-- Command line options
consoleOpts :: [OptDescr Flag]
consoleOpts = [Option ['i'] ["infer"] (NoArg InferType) "only infer type(s) of given program",
               Option ['f'] ["file"] (NoArg InFile) "read program from file",
               Option ['o'] ["out"] (ReqArg OutFile "FILE") "produce Haskell file of given name"]

main = do
  args <- getArgs
  let parsedArgs = getOpt RequireOrder consoleOpts args
  case parsedArgs of
    (opts, [input], []) -> do
      prog <- if InFile `elem` opts
              then readFile input
              else return input
      if InferType `elem` opts
        then case parseProg prog of
               Left err -> putStrLn err
               Right typings -> flip mapM_ typings $ \(typ, expr) ->
                                                       putStrLn $ show expr ++ " :: " ++ show typ
        else do
        let outfile = case (find isOutFile opts, InFile `elem` opts) of
                        (Just (OutFile s), _) -> s
                        (Nothing, True) -> input ++ ".hs"
                        (Nothing, False) -> ".out.hs"
        defs <- readFile "defs.hs"
        case parseProg prog of
          Left err -> putStrLn err
          Right [] -> putStrLn "Could not infer valid type"
          Right ((typ,expr):_) -> do
            writeFile outfile $ defs ++ "func = " ++ expToHaskell expr
            putStrLn $ "Wrote to " ++ outfile
    (_, _, errs) -> putStrLn $ concat errs ++ "Usage: main [OPTION...] [FILE|EXPR]"
