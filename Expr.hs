
-- Expressions and types

{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.List (intercalate)

-- Labels for type and expression variables
type TLabel = String
type ELabel = String

-- Expression containing generalized literals
data Exp lit = EVar ELabel
             | ELine Int
             | ELit lit
             | EApp (Exp lit) (Exp lit)
             | EOp (Exp lit) (Exp lit) (Exp lit)
             | EAbs ELabel (Exp lit)
             | ELet ELabel (Exp lit) (Exp lit)
  deriving (Eq, Ord, Functor)

instance (Show lit) => Show (Exp lit) where
  show (EVar name) = name
  show (ELine n) = "line" ++ show n
  show (ELit lit) = show lit
  show (EApp a b) = show a ++ "(" ++ show b ++ ")"
  show (EOp a b c) = show $ EApp (EApp a b) c
  show (EAbs name exp) = "(\\" ++ name ++ "." ++ show exp ++ ")"
  show (ELet name exp body) = "let " ++ name ++ "=(" ++ show exp ++ ") in " ++ show body

-- Literal in expression; t is the type
data Lit t = Value String t
           | Builtin String t
           | Vec t
  deriving (Eq, Ord)

instance Show (Lit a) where
  show (Value name _) = name
  show (Builtin name _) = name
  show (Vec _) = "vec"

-- Type of expression with unbound variables
data Type = TVar TLabel
          | TConc Conc
          | TList Type
          | TPair Type Type
          | TFun Type Type
  deriving (Eq, Ord)

-- Convenience alias for TFun
infixr 9 ~>
(~>) = TFun

instance Show Type where
  show (TVar name) = name
  show (TConc c) = show c
  show (TList t) = "[" ++ show t ++ "]"
  show (TPair a b) = "(" ++ show a ++ "," ++ show b ++ ")"
  show (TFun a b) = "(" ++ show a ++ "->" ++ show b ++ ")"

-- Concrete type
data Conc = TInt
          | TDouble
          | TChar
          | TNil
  deriving (Eq, Ord, Show)

-- Type with typeclass constraints
data CType = CType [TClass] Type
  deriving (Eq, Ord)

instance Show CType where
  show (CType cons typ) = show cons ++ "=>" ++ show typ

-- Possible typeclass constraints
data TClass = Vect Type Type Type Type
            | Concrete Type
            | Number Type
  deriving (Eq, Ord, Show)

-- Check typeclass constraint, return list of "simpler" constraints that are equivalent to it
-- Nothing ==> Constraint doesn't hold
-- Just [] ==> Constraint holds (equivalent to no constraints)
-- Just cs ==> Equivalent to all cs holding
holds :: TClass -> Maybe [TClass]
holds c@(Concrete (TVar _))    = Just [c]
holds (Concrete (TConc _))     = Just []
holds (Concrete (TList t))     = holds (Concrete t)
holds (Concrete (TPair t1 t2)) = do
  h1 <- holds (Concrete t1)
  h2 <- holds (Concrete t2)
  return $ h1 ++ h2
holds (Concrete (TFun _ _))    = Nothing

holds c@(Number (TVar _))      = Just [c]
holds (Number (TConc TInt))    = Just []
holds (Number (TConc TDouble)) = Just []
holds (Number _)               = Nothing

holds (Vect t1 t2 s1 s2) | s1 == t1, s2 == t2 = Just []
holds (Vect t1 t2 (TList s1) (TList s2))      = holds $ Vect t1 t2 s1 s2
holds c@(Vect _ _ (TVar _) (TList _))         = Just [c]
holds c@(Vect _ _ (TList _) (TVar _))         = Just [c]
holds c@(Vect _ _ (TVar _) (TVar _))          = Just [c]
holds (Vect _ _ _ _)                          = Nothing

-- Find a nesting depth at which list-nested t1 can be unified with t2
-- Can assume one exists
uniDepth :: Type -> Type -> Int
uniDepth t1 t2 | t1 == t2 = 0
uniDepth (TList t1) (TList t2) = uniDepth t1 t2
uniDepth t1 (TList t2) = 1 + uniDepth t1 t2
uniDepth _ _ = 0

-- Default typeclass instances, given as unifiable pairs of types
defInst :: TClass -> (Type, Type)
defInst (Concrete t)       = (t, TConc TInt)
defInst (Number t)         = (t, TConc TInt)
defInst (Vect t1 t2 s1 s2) = (TPair s1 s2,
                               TPair
                               (iterate TList t1 !! n)
                               (iterate TList t2 !! n))
  where n = max (uniDepth t1 s1) (uniDepth t2 s2)

-- Type of expression with universally quantified variables
data Scheme = Scheme [TLabel] CType
  deriving (Eq, Ord)

instance Show Scheme where
  show (Scheme vs t) = concatMap (\name -> "forall " ++ name ++ ".") vs ++ show t

-- Convert type to Haskell code
typeToHaskell :: Type -> String
typeToHaskell (TVar name) = name
typeToHaskell (TConc TInt) = "Integer"
typeToHaskell (TConc TDouble) = "Double"
typeToHaskell (TConc TChar) = "Char"
typeToHaskell (TConc TNil) = "()"
typeToHaskell (TList t) = "[" ++ typeToHaskell t ++ "]"
typeToHaskell (TPair s t) = "(" ++ typeToHaskell s ++ "," ++ typeToHaskell t ++ ")"
typeToHaskell (TFun s t) = "(" ++ typeToHaskell s ++ " -> " ++ typeToHaskell t ++ ")"

-- Convert typeclass constraint to Haskell code
consToHaskell :: TClass -> String
consToHaskell (Concrete t)       = "Concrete " ++ typeToHaskell t
consToHaskell (Number t)         = "Number " ++ typeToHaskell t
consToHaskell (Vect t1 t2 s1 s2) = "Number Integer" -- Dummy value

-- Convert classed type to Haskell code
cTypeToHaskell :: CType -> String
cTypeToHaskell (CType [] typ) = typeToHaskell typ
cTypeToHaskell (CType cons typ) = "(" ++ intercalate "," (map consToHaskell cons) ++ ") => " ++ typeToHaskell typ

-- Convert expression to Haskell code
expToHaskell :: Exp (Lit CType) -> String
expToHaskell (EVar name) = name
expToHaskell (ELine n) = "line" ++ show n
expToHaskell (ELit (Value name typ)) = "(" ++ name ++ "::" ++ cTypeToHaskell typ ++ ")"
expToHaskell (ELit (Builtin name typ)) = "(func_" ++ name ++ "::" ++ cTypeToHaskell typ ++ ")"
expToHaskell (ELit (Vec typ)) = vecToHaskell typ
expToHaskell (EApp a b) = "(" ++ expToHaskell a ++ ")(" ++ expToHaskell b ++ ")"
expToHaskell (EOp _ _ _) = error "expToHaskell not defined for EOp"
expToHaskell (EAbs name exp) = "(\\ " ++ name ++ " -> " ++ expToHaskell exp ++ ")"
expToHaskell (ELet name exp body) = "(let " ++ name ++ " = " ++ expToHaskell exp ++ " in " ++ expToHaskell body ++ ")"

-- Convert type of Vec to Haskell expression (nested maps)
-- Type will always be of the form (a -> b) -> (x -> y)
vecToHaskell typ@(CType _ (TFun (TFun a b) (TFun x y))) = "(id" ++ concat (replicate (nesting x) ".fmap") ++ "::" ++ cTypeToHaskell typ ++ ")"
  where nesting t | t == a = 0
                  | TList t' <- t = 1 + nesting t'
                  | otherwise = error "Illegal type for Vec"
