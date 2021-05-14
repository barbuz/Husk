{-# LANGUAGE QuasiQuotes #-}

-- Main program

import Debug
import Expr
import Infer
import Parser
import InputParser
import Codepage
import Codegen
import FileQuoter
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Process
import System.IO
import qualified Data.ByteString as B
import Data.List (find, intercalate, nub)
import Data.Set (toAscList)

-- Wrapper for expression parser
parseProg :: Bool -> String -> [Type] -> Either String [[(Int, CType, Exp (Lit CType))]]
parseProg constrainRes prog types = inferType constrainRes (foldr typeConstr resType types) <$> parseExpr prog
  where typeConstr typ1 (Scheme vars (CType cons typ2)) =
          Scheme (nub $ vars ++ toAscList (freeVars typ1)) $
          CType cons $
          TFun typ1 typ2
        cons = if constrainRes then [Concrete $ TVar "x"] else []
        resType = Scheme ["x"] $ CType cons $ TVar "x"

-- Input format flags
data Format = Bytes
            | Unicode
            | Verbose
            deriving (Eq, Show)

-- Command line option flags
data Flag = InferType
          | InferInputType
          | InFile
          | OutFile String
          | Format Format
          | Translate Format
          deriving (Eq, Show)

isOutFile :: Flag -> Bool
isOutFile (OutFile _) = True
isOutFile _ = False

isFormat :: Flag -> Bool
isFormat (Format _) = True
isFormat _ = False

isTranslate :: Flag -> Bool
isTranslate (Translate _) = True
isTranslate _ = False

-- Command line options
consoleOpts :: [OptDescr Flag]
consoleOpts = [Option ['b'] ["bytes"] (NoArg $ Format Bytes) "take input as bytes",
               Option ['u'] ["unicode"] (NoArg $ Format Unicode) "take input as Unicode characters",
               Option ['v'] ["verbose"] (NoArg $ Format Verbose) "take input as verbose ASCII",
               Option ['i'] ["infer"] (NoArg InferType) "only infer type(s) of given program",
               Option ['I'] ["infer2"] (NoArg InferInputType) "infer type(s) of given program, taking input type(s) into account",
               Option ['f'] ["file"] (NoArg InFile) "read program from file",
               Option ['o'] ["out"] (ReqArg OutFile "FILE") "produce Haskell file of given name",
               Option ['t'] ["translate"] (ReqArg (Translate . parseFormat) "FORMAT") "translate source to specified format (b/u/v)"
               ]
  where parseFormat "b" = Bytes
        parseFormat "u" = Unicode
        parseFormat "v" = Verbose
        parseFormat _ = error "Bad format specifier"

-- Imports needed for transpiled file
fileImports :: String
fileImports = unlines $ map ("import "++) $ ["Defs", "IntSeq", "System.Environment (getArgs)"]

-- Produce Haskell file from list of type-inferred lines
produceFile :: [(Int, CType, Exp (Lit CType))] -> String
produceFile exprs =
  fileImports ++
  progLines ++
  "main :: IO ()\n" ++
  "main = do{[" ++ intercalate "," argList ++ "] <- getArgs; " ++
  "let{res = line0 " ++ concatMap (\a -> "(read " ++ a ++ ")") argList ++ "}; " ++
  "putStr (toString res)}"
  where progLines = concat [ label ++ " :: " ++ cTypeToHaskell typ ++ "\n" ++
                             label ++ " = " ++ expToHaskell expr ++ "\n"
                           | (i, typ, expr) <- exprs,
                             let label = "line" ++ show i]
        (_, CType _ mainTyp, _) = exprs !! 0
        argList = ["arg" ++ show i | i <- [1..numArgs mainTyp]]
        numArgs (TFun _ t) = 1 + numArgs t
        numArgs _ = 0

main = do
  args <- getArgs
  let parsedArgs = getOpt RequireOrder consoleOpts args
  case parsedArgs of
    (opts, (progOrFile : progArgs), []) -> traceShow' 1 opts $ do
      errOrProg <- if InFile `elem` opts
                   then case find isFormat opts of
                          Just (Format Bytes) -> Right . getCommands . B.unpack <$> B.readFile progOrFile
                          Just (Format f) -> do
                            handle <- openFile progOrFile ReadMode
                            hSetEncoding handle utf8
                            contents <- hGetContents handle
                            return $ case f of
                              Verbose -> parseAliases contents
                              _ -> Right contents
                          _ -> return $ Left "Must supply input format"
                   else return $ case find isFormat opts of
                                   Just (Format Bytes)   -> Left "Byte format not supported for console input"
                                   Just (Format Verbose) -> parseAliases progOrFile
                                   Just (Format Unicode) -> Right progOrFile
                                   _ -> Left "Must supply input format"
      case errOrProg of
        Left err   -> putStrLn err
        Right prog ->
          let progInputs :: Either String (Maybe [(String,Type)])
              progInputs = fmap sequence $ sequence $ zipWith parseInput [1..] progArgs
          in case progInputs of
               Left err          -> putStrLn err
               Right Nothing     -> putStrLn "Could not infer valid type(s) for input(s)"
               Right (Just typedArgs) ->
                 if any (`elem` opts) [InferType, InferInputType]
                 then let constrainType = InferInputType `elem` opts
                          inputs = if InferInputType `elem` opts then map snd typedArgs else []
                      in case parseProg constrainType prog inputs of
                           Left err -> putStrLn err
                           Right typings -> flip mapM_ typings $
                                            \exprs -> do
                                              putStrLn "%%%%"
                                              flip mapM_ exprs $
                                                \(i, typ, expr) -> do
                                                  putStrLn $ "line" ++ show i ++ " = " ++ show expr ++ " :: " ++ show typ
                 else case find isTranslate opts of
                        Just (Translate Verbose) -> putStrLn $ toAliases prog
                        Just (Translate Unicode) -> putStrLn prog
                        Just (Translate Bytes)   ->
                          let bytes = B.pack $ getBytes prog
                          in case find isOutFile opts of
                               Just (OutFile filename) -> B.writeFile filename bytes
                               _ -> B.putStr bytes
                        _ -> do
                          let outfile = case (find isOutFile opts, InFile `elem` opts) of
                                          (Just (OutFile s), _) -> s
                                          (Nothing, True) -> progOrFile ++ ".hs"
                                          (Nothing, False) -> ".out.hs"
                          case parseProg True prog (map snd typedArgs) of
                            Left err             -> putStrLn err
                            Right []             -> putStrLn "Could not infer valid type for program"
                            Right (lineExprs : _) -> do writeFile outfile $ produceFile lineExprs
                                                        (_, Just hout, _, _) <- createProcess (proc "runhaskell" (outfile : map fst typedArgs)){ std_out = CreatePipe }
                                                        result <- hGetContents hout
                                                        hSetBuffering stdout NoBuffering
                                                        putStr result
    (_, _, errs) -> putStrLn $ concat errs ++ usageInfo "Usage: main [OPTION...] [FILE|EXPR] [INPUT...]" consoleOpts
