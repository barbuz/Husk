
module Parser where

import Expr
import PrattParser
import Text.Parsec
import qualified Data.Map as Map

-- Convenience alias for TFun
infixr 9 ~>
(~>) = TFun

-- Unwrapped parser, giving strings for errors
parseExpr :: String -> Either String (Exp [Lit])
parseExpr str = case parse expression "" str of
  Left err -> Left $ show err
  Right val -> Right val

-- Parse an expression
expression :: Parsec String () (Exp [Lit])
expression = mkPrattParser opTable term
  where term = between (char '(') (char ')') expression <|> builtin <|> integer
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
builtin :: Parsec String () (Exp [Lit])
builtin = do
  label <- oneOf $ map fst builtins
  case lookup label builtins of
    Just expr -> return expr
    Nothing -> error "Unreachable condition."

-- Parse an integer
integer :: Parsec String () (Exp [Lit])
integer = do
  i <- many1 digit
  return $ ELit [Lit i $ Scheme [] $ TConc TInt]
