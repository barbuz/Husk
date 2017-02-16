
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
  where term = between (char '(') rParen expression <|> builtin <|> integer <|> lambda <|> lambdaArg
        opTable = [[InfixL $ optional (char ' ') >> return (\a b -> EApp (EApp invisibleOp a) b)]]
        invisibleOp = ELit [Lit "com2" $ Scheme ["x", "y", "z", "u"] $
                             (TVar "z" ~> TVar "u") ~>
                             (TVar "x" ~> TVar "y" ~> TVar "z") ~>
                             (TVar "x" ~> TVar "y" ~> TVar "u"),
                            Lit "com"  $ Scheme ["x", "y", "z"] $
                             (TVar "y" ~> TVar "z") ~>
                             (TVar "x" ~> TVar "y") ~>
                             (TVar "x" ~> TVar "z"),
                            Lit "app"  $ Scheme ["x", "y"] $
                             (TVar "x" ~> TVar "y") ~>
                             (TVar "x" ~> TVar "y")]

-- List of builtin commands
builtins :: [(Char, Exp [Lit])]
builtins = map (fmap ELit)
  [('+', [Lit "add"  $ Scheme [] $ TConc TInt ~> TConc TInt ~> TConc TInt]),
   ('-', [Lit "sub"  $ Scheme [] $ TConc TInt ~> TConc TInt ~> TConc TInt]),
   ('_', [Lit "neg"  $ Scheme [] $ TConc TInt ~> TConc TInt]),
   ('*', [Lit "mul"  $ Scheme [] $ TConc TInt ~> TConc TInt ~> TConc TInt]),
   (';', [Lit "pure" $ Scheme ["x"] $ TVar "x" ~> TList (TVar "x")]),
   (':', [Lit "pair" $ Scheme ["x"] $ TVar "x" ~> TVar "x" ~> TList (TVar "x"),
          Lit "cons" $ Scheme ["x"] $ TVar "x" ~> TList (TVar "x") ~> TList (TVar "x"),
          Lit "snoc" $ Scheme ["x"] $ TList (TVar "x") ~> TVar "x" ~> TList (TVar "x"),
          Lit "cat"  $ Scheme ["x"] $ TList (TVar "x") ~> TList (TVar "x") ~> TList (TVar "x")]),
   ('m', [Lit "map"  $ Scheme ["x", "y"] $
           (TVar "x" ~> TVar "y") ~>
           (TList (TVar "x") ~> TList (TVar "y")),
          Lit "zip"  $ Scheme ["x", "y", "z"] $
           (TVar "x" ~> TVar "y" ~> TVar "z") ~>
           (TList (TVar "x") ~> TList (TVar "y") ~> TList (TVar "z"))])]

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
  return $ ELit [Lit i $ Scheme [] $ TConc TInt]

-- Parse a lambda
lambda :: Parser (Exp [Lit])
lambda = do
  char 'λ'
  var <- pushNewVar
  expr <- expression
  popVar
  rParen
  return $ EAbs var expr

-- Parse a lambda argument
lambdaArg :: Parser (Exp [Lit])
lambdaArg = do
  sup <- oneOf sups
  let Just ix = elemIndex sup sups
  var <- peekVar ix
  return $ EVar var
  where sups = "¹²³⁴⁵⁶⁷⁸⁹"