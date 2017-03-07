
module Parser where

import Expr
import PrattParser
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Map as Map
import Data.List (elemIndex)

-- Convenience alias for TFun
infixr 9 ~>
(~>) = TFun

-- Parser state
data PState = PState {varStack :: [ELabel],
                      varSupply :: Int}

-- Parser type
type Parser = Parsec String PState

-- Unwrapped parser, giving strings for errors
parseExpr :: String -> Either String (Exp [Lit])
parseExpr str = case runParser expression initState "" str of
  Left err -> Left $ show err
  Right val -> Right val
  where initState = PState [] 0

-- Generate and push a new expression variable
pushNewVar :: Parser ELabel
pushNewVar = do
  stat <- getState
  let var = "x" ++ show (varSupply stat)
  putState stat{varStack = var : varStack stat,
                varSupply = varSupply stat + 1}
  return var

-- Peek at a variable from the stack
peekVar :: Int -> Parser ELabel
peekVar i = (!! i) . varStack <$> getState

-- Pop a variable off the stack
popVar :: Parser ()
popVar = do
  stat <- getState
  putState stat{varStack = tail $ varStack stat}

-- Parse a right paren or be at end of line
rParen :: Parser ()
rParen = (char ')' >> return ()) <|> (lookAhead endOfLine >> return ()) <|> lookAhead eof

-- Parse an expression
expression :: Parser (Exp [Lit])
expression = mkPrattParser opTable term
  where term = between (char '(') rParen expression <|> builtin <|> integer <|> character <|> str <|> lambda <|> lambdaArg
        opTable = [[InfixL $ optional (char ' ') >> return (\a b -> EApp (EApp invisibleOp a) b)]]
        invisibleOp = ELit [Lit "com3" $ Scheme ["x", "y", "z", "u", "v"] $ CType [] $
                             (TVar "u" ~> TVar "v") ~>
                             (TVar "x" ~> TVar "y" ~> TVar "z" ~> TVar "u") ~>
                             (TVar "x" ~> TVar "y" ~> TVar "z" ~> TVar "v"),
                            Lit "com2" $ Scheme ["x", "y", "z", "u"] $ CType [] $
                             (TVar "z" ~> TVar "u") ~>
                             (TVar "x" ~> TVar "y" ~> TVar "z") ~>
                             (TVar "x" ~> TVar "y" ~> TVar "u"),
                            Lit "com"  $ Scheme ["x", "y", "z"] $ CType [] $
                             (TVar "y" ~> TVar "z") ~>
                             (TVar "x" ~> TVar "y") ~>
                             (TVar "x" ~> TVar "z"),
                            Lit "app"  $ Scheme ["x", "y"] $ CType [] $
                             (TVar "x" ~> TVar "y") ~>
                             (TVar "x" ~> TVar "y")]

-- List of builtin commands
builtins :: [(Char, Exp [Lit])]
builtins = map (fmap ELit)
  [('+', [Lit "add"  $ Scheme [] $ CType [] $ TConc TInt ~> TConc TInt ~> TConc TInt]),
   ('-', [Lit "sub"  $ Scheme [] $ CType [] $ TConc TInt ~> TConc TInt ~> TConc TInt]),
   ('_', [Lit "neg"  $ Scheme [] $ CType [] $ TConc TInt ~> TConc TInt]),
   ('*', [Lit "mul"  $ Scheme [] $ CType [] $ TConc TInt ~> TConc TInt ~> TConc TInt]),
   (';', [Lit "pure" $ Scheme ["x"] $ CType [] $ TVar "x" ~> TList (TVar "x")]),
   (':', [Lit "pair" $ Scheme ["x"] $ CType [] $ TVar "x" ~> TVar "x" ~> TList (TVar "x"),
          Lit "cons" $ Scheme ["x"] $ CType [] $ TVar "x" ~> TList (TVar "x") ~> TList (TVar "x"),
          Lit "cat"  $ Scheme ["x"] $ CType [] $ TList (TVar "x") ~> TList (TVar "x") ~> TList (TVar "x"),
          Lit "snoc" $ Scheme ["x"] $ CType [] $ TList (TVar "x") ~> TVar "x" ~> TList (TVar "x")]),
   ('m', [Lit "map"  $ Scheme ["x", "y"] $ CType [] $
           (TVar "x" ~> TVar "y") ~>
           (TList (TVar "x") ~> TList (TVar "y"))]),
   ('z', [Lit "zip"  $ Scheme ["x", "y", "z"] $ CType [] $
           (TVar "x" ~> TVar "y" ~> TVar "z") ~>
           (TList (TVar "x") ~> TList (TVar "y") ~> TList (TVar "z"))]),
   ('F', [Lit "fixp" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $
                       (TVar "x" ~> TVar "x") ~> TVar "x" ~> TVar "x"]),
   ('<', [Lit "lt"   $ Scheme ["x"] $ CType [Concrete (TVar "x")] $
                       TVar "x" ~> TVar "x" ~> TConc TInt]),
   ('>', [Lit "gt"   $ Scheme ["x"] $ CType [Concrete (TVar "x")] $
                       TVar "x" ~> TVar "x" ~> TConc TInt]),
   ('=', [Lit "eq"   $ Scheme ["x"] $ CType [Concrete (TVar "x")] $
                       TVar "x" ~> TVar "x" ~> TConc TInt]),
   ('?', [Lit "if"   $ Scheme ["x", "y"] $ CType [Concrete (TVar "x")] $
                       TVar "x" ~> TVar "y" ~> TVar "y" ~> TVar "y"])
  ]

-- Parse a builtin
builtin :: Parser (Exp [Lit])
builtin = do
  label <- oneOf $ map fst builtins
  case lookup label builtins of
    Just expr -> return expr
    Nothing -> error "Unreachable condition."

-- Parse an integer
integer :: Parser (Exp [Lit])
integer = do
  i <- many1 digit
  return $ ELit [Lit i $ Scheme [] $ CType [] $ TConc TInt]
 
-- Parse a character
character :: Parser (Exp [Lit])
character = do
  quote <- char '\''
  c <- anyChar
  return $ ELit [Lit (show c) $ Scheme [] $ CType [] $ TConc TChar]

-- Parse a string
str :: Parser (Exp [Lit])
str = do
  quote <- char '"'
  s <- many $ noneOf "\""
  quote2 <- char '"'
  return $ ELit [Lit (show s) $ Scheme [] $ CType [] $ TList (TConc TChar)]

-- Parse a generalized lambda
lambda :: Parser (Exp [Lit])
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
  return $ if lam `elem` "φψχ" then EApp fixExpr expr else expr
  where
    lambdify parser = do
      var <- pushNewVar
      expr <- parser
      popVar
      return $ EAbs var expr
    fixExpr = ELit [Lit "fix" $ Scheme ["x"] $ CType [] $ (TVar "x" ~> TVar "x") ~> TVar "x"]

-- Parse a lambda argument
lambdaArg :: Parser (Exp [Lit])
lambdaArg = do
  sup <- oneOf sups
  let Just ix = elemIndex sup sups
  var <- peekVar ix
  return $ EVar var
  where sups = "¹²³⁴⁵⁶⁷⁸⁹"
