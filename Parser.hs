
module Parser where

import Expr
import Builtins
import PrattParser
import Text.Parsec
import Text.Parsec.Char
import Control.Monad (forM)
import qualified Data.Map as Map
import Data.List (elemIndex)

-- Parser state
data PState = PState {varStack :: [ELabel],
                      varSupply :: Int,
                      lineNum :: Int}

-- Parser type
type Parser = Parsec String PState

-- Unwrapped parser, giving strings for errors
parseExpr :: String -> Either String (Exp [Lit Scheme])
parseExpr str = case runParser multiline initState "" str of
  Left err -> Left $ show err
  Right val -> Right val
  where initState = PState [] 0 1

-- Generate and push a new expression variable
pushNewVar :: Parser ELabel
pushNewVar = do
  stat <- getState
  let var = "x" ++ show (varSupply stat)
  putState stat{varStack = var : varStack stat,
                varSupply = varSupply stat + 1}
  return var

-- Peek at a variable from the stack; extend stack if necessary
peekVar :: Int -> Parser ELabel
peekVar ix = do
  len <- length . varStack <$> getState
  if ix >= len
    then do
      vars <- forM [0..ix-len] $ const pushNewVar
      return $ head vars
    else (!! ix) . varStack <$> getState

-- Pop a variable off the stack
popVar :: Parser ()
popVar = do
  stat <- getState
  putState stat{varStack = tail $ varStack stat}

-- Parse a right paren or be at end of line
rParen :: Parser ()
rParen = (char ')' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof

-- Parse a multiline expression; first line is "main line"
multiline :: Parser (Exp [Lit Scheme])
multiline = do
  lines <- sepBy1 parseLine endOfLine
  eof
  let (_, folded) = foldr1 (\(num1, expr1) (num2, expr2) -> (num1, ELet ("sub" ++ show num2) expr2 expr1)) lines
  return $ ELet "sub1" folded $ EVar "sub1"
  where parseLine :: Parser (Int, Exp [Lit Scheme])
        parseLine = do state <- getState
                       putState state{lineNum = lineNum state + 1}
                       lineExpr $ lineNum state

-- Parse a line of Husk code
lineExpr :: Int -> Parser (Int, Exp [Lit Scheme])
lineExpr lineNum = do
  state <- getState
  putState state{varStack = []}
  expr <- expression
  overflowVars <- reverse . varStack <$> getState
  let lambdified = foldr EAbs expr overflowVars
  return (lineNum, lambdified)

-- Parse an expression
expression :: Parser (Exp [Lit Scheme])
expression = mkPrattParser opTable term
  where term = between (char '(') rParen expression <|> builtin <|> number <|> character <|> str <|> lambda <|> lambdaArg <|> subscript
        opTable = [[InfixL $ optional (char ' ') >> return (EOp invisibleOp)]]
        invisibleOp = bins "com3 com2 com app"

-- Parse a builtin
builtin :: Parser (Exp [Lit Scheme])
builtin = do
  label <- oneOf commands
  return $ cmd label

-- Parse a number (integer or float)
number :: Parser (Exp [Lit Scheme])
number = do
  prefix <- many1 digit
  maybeSuffix <- optionMaybe $ char '.' >> many digit
  case maybeSuffix of
    Nothing     -> return $ ELit [Lit prefix $ Scheme ["n"] $ CType [(Number, TVar "n")] $ TVar "n"]
    Just []     -> return $ ELit [Lit (prefix ++ ".0") $ Scheme [] $ CType [] $ TConc TDouble]
    Just suffix -> return $ ELit [Lit (prefix ++ "." ++ suffix) $ Scheme [] $ CType [] $ TConc TDouble]
 
-- Parse a character
character :: Parser (Exp [Lit Scheme])
character = do
  quote <- char '\''
  c <- anyChar
  return $ ELit [Lit (show c) $ Scheme [] $ CType [] $ TConc TChar]

-- Parse a string
str :: Parser (Exp [Lit Scheme])
str = do
  quote <- char '"'
  s <- many $ noneOf "\""
  quote2 <- char '"'
  return $ ELit [Lit (show s) $ Scheme [] $ CType [] $ TList (TConc TChar)]

-- Parse a generalized lambda
lambda :: Parser (Exp [Lit Scheme])
lambda = do
  lam <- oneOf "λμκφψχ"
  let numArgs = case lam of
        'λ' -> 1
        'μ' -> 2
        'κ' -> 3
        'φ' -> 1
        'ψ' -> 2
        'χ' -> 3
  expr <- iterate lambdify expression !! numArgs
  rParen
  return $ if lam `elem` "φψχ" then EApp (bins "fix") expr else expr
  where
    lambdify parser = do
      var <- pushNewVar
      expr <- parser
      popVar
      return $ EAbs var expr

-- Parse a lambda argument
lambdaArg :: Parser (Exp [Lit Scheme])
lambdaArg = do
  sup <- oneOf sups
  let Just ix = elemIndex sup sups
  var <- peekVar ix
  return $ EVar var
  where sups = "¹²³⁴⁵⁶⁷⁸⁹"

-- Parse a subscript; used as line numbers and built-in constants
subscript :: Parser (Exp [Lit Scheme])
subscript = do
  sub <- oneOf subs
  let Just ix = elemIndex sub subs
  return $ EVar ("sub" ++ show (ix + 1))
  where subs = "₁₂₃₄₅₆₇₈₉"
