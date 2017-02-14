
-- Main program

import Expr
import Infer
import Parser

-- Wrapper for expression parser
parseProg :: String -> Either String [(Type, Exp Lit)]
parseProg s = inferType <$> parseExpr s

main = do
  prog <- getLine
  case parseProg prog of
    Left err -> putStrLn err
    Right typings -> flip mapM_ typings $ \(typ, expr) -> putStrLn $ show expr ++ " :: " ++ show typ
