
-- Expressions and types

module Expr where

-- Labels for type and expression variables
type TLabel = String
type ELabel = String

-- Expression containing generalized literals
data Exp lit = EVar ELabel
             | ELit lit
             | EApp (Exp lit) (Exp lit)
             | EAbs ELabel (Exp lit)
             | ELet ELabel (Exp lit) (Exp lit)
  deriving (Eq, Ord)

instance (Show lit) => Show (Exp lit) where
  show (EVar name) = name
  show (ELit lit) = show lit
  show (EApp a b) = show a ++ "(" ++ show b ++ ")"
  show (EAbs name exp) = "(\\" ++ name ++ "." ++ show exp ++ ")"
  show (ELet name exp body) = "let " ++ name ++ "=" ++ show exp ++ " in " ++ show body

-- Literal in expression
data Lit = Lit String Scheme
  deriving (Eq, Ord)

instance Show Lit where
  show (Lit n s) = n

-- Convert expression to Haskell code
expToHaskell :: Exp Lit -> String
expToHaskell (EVar name) = name
expToHaskell (ELit (Lit name typ))
  | Scheme _ (TFun _ _) <- typ = "func_" ++ name
  | otherwise = name
expToHaskell (EApp a b) = "(" ++ expToHaskell a ++ ")(" ++ expToHaskell b ++ ")"
expToHaskell (EAbs name exp) = "(\\ " ++ name ++ " -> " ++ expToHaskell exp ++ ")"
expToHaskell (ELet name exp body) = "(let " ++ name ++ " = " ++ expToHaskell exp ++ " in " ++ expToHaskell body ++ ")"

-- Type of expression with unbound variables
data Type = TVar TLabel
          | TConc Conc
          | TList Type
          | TFun Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVar name) = name
  show (TConc c) = show c
  show (TList t) = "[" ++ show t ++ "]"
  show (TFun a b) = "(" ++ show a ++ "->" ++ show b ++ ")"

-- Concrete type
data Conc = TInt
          | TBool
  deriving (Eq, Ord, Show)

-- Type of expression with universally quantified variables
data Scheme = Scheme [TLabel] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Scheme vs t) = concatMap (\name -> "forall " ++ name ++ ".") vs ++ show t

