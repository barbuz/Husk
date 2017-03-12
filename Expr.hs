
-- Expressions and types

module Expr where

import Data.List (intercalate)

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
  show (ELet name exp body) = "let " ++ name ++ "=(" ++ show exp ++ ") in " ++ show body

-- Literal in expression
data Lit = Lit String Scheme
  deriving (Eq, Ord)

instance Show Lit where
  show (Lit n s) = n

-- Convert expression to Haskell code
expToHaskell :: Exp Lit -> String
expToHaskell (EVar name) = name
expToHaskell (ELit (Lit name typ))
  | Scheme _ (CType _ (TFun _ _)) <- typ = "func_" ++ name
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
          | TDouble
          | TChar
  deriving (Eq, Ord, Show)

-- Type with typeclass constraints
data CType = CType [(TClass, Type)] Type
  deriving (Eq, Ord)

instance Show CType where
  show (CType cons typ) = show cons ++ "=>" ++ show typ

-- Possible typeclass constraints
data TClass = Concrete
            | Number
  deriving (Eq, Ord, Show)

-- Check typeclass constraint, return list of "simpler" constraints that are equivalent to it
-- Nothing ==> Constraint doesn't hold
-- Just [] ==> Constraint holds (equivalent to no constraints)
-- Just cs ==> Equivalent to all cs holding
holds :: (TClass, Type) -> Maybe [(TClass, Type)]
holds c@(Concrete, TVar _) = Just [c]
holds (Concrete, TConc _)  = Just []
holds (Concrete, TList t)  = holds (Concrete, t)
holds (Concrete, TFun _ _) = Nothing
holds c@(Number, TVar _)      = Just [c]
holds (Number, TConc TInt)    = Just []
holds (Number, TConc TDouble) = Just []
holds (Number, _)             = Nothing

-- Default typeclass instances
defInst :: TClass -> Type
defInst Concrete = TConc TInt
defInst Number   = TConc TInt

-- Convert type to Haskell code
typeToHaskell :: Type -> String
typeToHaskell (TVar name) = name
typeToHaskell (TConc TInt) = "Integer"
typeToHaskell (TConc TDouble) = "Double"
typeToHaskell (TConc TChar) = "Char"
typeToHaskell (TList t) = "[" ++ typeToHaskell t ++ "]"
typeToHaskell (TFun s t) = "(" ++ typeToHaskell s ++ " -> " ++ typeToHaskell t ++ ")"

-- Convert classed type to Haskell code
cTypeToHaskell :: CType -> String
cTypeToHaskell (CType [] typ) = typeToHaskell typ
cTypeToHaskell (CType cons typ) = "(" ++ intercalate "," (map showCons cons) ++ ") => " ++ typeToHaskell typ
  where showCons (con, typ) = show con ++ " " ++ typeToHaskell typ

-- Type of expression with universally quantified variables
data Scheme = Scheme [TLabel] CType
  deriving (Eq, Ord)

instance Show Scheme where
  show (Scheme vs t) = concatMap (\name -> "forall " ++ name ++ ".") vs ++ show t

