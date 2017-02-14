
module Parser where

import Infer
import PrattParser
import Text.Parsec

(~>) = TFun

expression :: Parsec String () (Exp [Lit])
expression = mkPrattParser opTable term
  where term = between (char '(') (char ')') expression <|> builtin <|> integer
        opTable = [[InfixL $ return (\a b -> EApp (EApp invisibleOp a) b)]]
        invisibleOp = ELit [Lit "com2" $ Scheme ["x", "y", "z", "u"] $ (TVar "z" ~> TVar "u") ~> ((TVar "x" ~> (TVar "y" ~> TVar "z")) ~> (TVar "x" ~> (TVar "y" ~> TVar "u"))),
                            Lit "com"  $ Scheme ["x", "y", "z"] $ (TVar "y" ~> TVar "z") ~> ((TVar "x" ~> TVar "y") ~> (TVar "x" ~> TVar "z")),
                            Lit "app"  $ Scheme ["x", "y"] $ (TVar "x" ~> TVar "y") ~> (TVar "x" ~> TVar "y")]

builtin :: Parsec String () (Exp [Lit])
builtin = do
  label <- oneOf "+-*:m"
  return $ ELit $ case label of
    '+' -> [Lit "add"  $ Scheme [] $ TConc TInt ~> (TConc TInt ~> TConc TInt)]
    '-' -> [Lit "sub"  $ Scheme [] $ TConc TInt ~> (TConc TInt ~> TConc TInt),
            Lit "neg"  $ Scheme [] $ TConc TInt ~> TConc TInt]
    '*' -> [Lit "mul"  $ Scheme [] $ TConc TInt ~> (TConc TInt ~> TConc TInt)]
    ':' -> [Lit "pure" $ Scheme ["x"] $ TVar "x" ~> TList (TVar "x"),
            Lit "pair" $ Scheme ["x"] $ TVar "x" ~> (TVar "x" ~> TList (TVar "x")),
            Lit "cons" $ Scheme ["x"] $ TVar "x" ~> (TList (TVar "x") ~> TList (TVar "x")),
            Lit "snoc" $ Scheme ["x"] $ TList (TVar "x") ~> (TVar "x" ~> TList (TVar "x")),
            Lit "cat"  $ Scheme ["x"] $ TList (TVar "x") ~> (TList (TVar "x") ~> TList (TVar "x"))]
    'm' -> [Lit "map"  $ Scheme ["x", "y"] $ (TVar "x" ~> TVar "y") ~> (TList (TVar "x") ~> TList (TVar "y")),
            Lit "zip"  $ Scheme ["x", "y", "z"] $ (TVar "x" ~> (TVar "y" ~> TVar "z")) ~>
                                                  (TList (TVar "x") ~> (TList (TVar "y") ~> TList (TVar "z")))]

integer :: Parsec String () (Exp [Lit])
integer = do
  i <- many1 digit
  return $ ELit [Lit i $ Scheme [] $ TConc TInt]

parseProg :: String -> [(Type, Exp Lit)]
parseProg s = inferType e
  where Right e = parse expression "" s
