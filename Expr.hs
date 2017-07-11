
-- Expressions and types

{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Debug
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
data Conc = TNum
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
  deriving (Eq, Ord, Show)

-- Possible results for enforcing a typeclass
data Enforce = Enforce {otherCons :: [TClass],       -- "simpler" typeclass constraints
                        otherUnis :: [(Type, Type)]} -- types to be unified

-- Find a nesting depth at which list-nested t1 equals t2
eqDepth :: Type -> Type -> Maybe Int
eqDepth t1 t2 | t1 == t2 = Just 0
eqDepth (TList t1) (TList t2) = eqDepth t1 t2
eqDepth t1 (TList t2) = succ <$> eqDepth t1 t2
eqDepth _ _ = Nothing

-- Find a nesting depth at which list-nested t1 could possibly be unified with t2
uniDepth :: Type -> Type -> Maybe Int
uniDepth t1 t2 | unifiable t1 t2 = Just 0
  where unifiable (TVar _) _ = True
        unifiable _ (TVar _) = True
        unifiable t1@(TConc _) t2@(TConc _) = t1 == t2
        unifiable (TPair l1 r1) (TPair l2 r2) = unifiable l1 l2 && unifiable r1 r2
        unifiable (TList t1) (TList t2) = unifiable t1 t2
        unifiable (TFun a1 r1) (TFun a2 r2) = unifiable a1 a2 && unifiable r1 r2
        unifiable _ _ = False
uniDepth (TList t1) (TList t2) = uniDepth t1 t2
uniDepth t1 (TList t2) = succ <$> uniDepth t1 t2
uniDepth _ _ = Nothing

-- Check typeclass constraint, return constraints and unifications to be enforced
-- "Nothing" means the constraint failed
holds :: TClass -> Maybe Enforce
holds c@(Concrete (TVar _))    = Just $ Enforce [c] []
holds (Concrete (TConc _))     = Just $ Enforce [] []
holds (Concrete (TList t))     = holds (Concrete t)
holds (Concrete (TPair t1 t2)) = do
  Enforce h1 _ <- holds (Concrete t1)
  Enforce h2 _ <- holds (Concrete t2)
  return $ Enforce (h1 ++ h2) []
holds (Concrete (TFun _ _))    = Nothing

holds (Vect t1 t2 s1 s2) | s1 == t1, s2 == t2 = Just $ Enforce [] []
holds c@(Vect t1 t2 s1 s2)
  | Nothing <- uniDepth t1 s1 = Nothing
  | Nothing <- uniDepth t2 s2 = Nothing
  | Just n <- eqDepth t1 s1 = Just $ Enforce [] [(iterate TList t2 !! n, s2)]
  | Just n <- eqDepth t2 s2 = Just $ Enforce [] [(iterate TList t1 !! n, s1)]
  | otherwise = Just $ Enforce [c] []

-- Default typeclass instances, given as unifiable pairs of types
defInst :: TClass -> (Type, Type)
defInst (Concrete t)       = (t, TConc TNum)
defInst (Vect t1 t2 s1 s2) = (TPair s1 s2,
                               TPair
                               (iterate TList t1 !! max 0 (n2 - n1))
                               (iterate TList t2 !! max 0 (n1 - n2)))
  where Just n1 = uniDepth t1 s1
        Just n2 = uniDepth t2 s2

-- Type of expression with universally quantified variables
data Scheme = Scheme [TLabel] CType
  deriving (Eq, Ord)

instance Show Scheme where
  show (Scheme vs t) = concatMap (\name -> "forall " ++ name ++ ".") vs ++ show t

-- Convert type to Haskell code
typeToHaskell :: Type -> String
typeToHaskell (TVar name) = name
typeToHaskell (TConc TNum) = "TNum"
typeToHaskell (TConc TChar) = "Char"
typeToHaskell (TConc TNil) = "()"
typeToHaskell (TList t) = "[" ++ typeToHaskell t ++ "]"
typeToHaskell (TPair s t) = "(" ++ typeToHaskell s ++ "," ++ typeToHaskell t ++ ")"
typeToHaskell (TFun s t) = "(" ++ typeToHaskell s ++ " -> " ++ typeToHaskell t ++ ")"

-- Convert typeclass constraint to Haskell code
consToHaskell :: TClass -> String
consToHaskell (Concrete t)       = "Concrete " ++ typeToHaskell t
consToHaskell (Vect t1 t2 s1 s2) = "Num Int" -- Dummy value

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
