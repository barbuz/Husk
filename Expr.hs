
-- Expressions and types

{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Debug

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
  show (EApp a b) | show a == "app" = show b
                  | show a `elem` words "com4 com3 com2 com" = "(" ++ show b ++ ")`" ++ show a ++ "`"
                  | otherwise       = show a ++ "(" ++ show b ++ ")"
  show (EOp a b c) = show $ EApp (EApp a b) c
  show (EAbs name exp) = "(\\" ++ name ++ "." ++ show exp ++ ")"
  show (ELet name exp body) = "let " ++ name ++ "=(" ++ show exp ++ ") in " ++ show body

-- Literal in expression; t is the type
data Lit t = Value String t
           | Builtin String t
           | Vec t
           | Vec2 Bool t -- True means zip', False means zip
  deriving (Eq, Ord)

instance Show (Lit a) where
  show (Value name _) = name
  show (Builtin name _) = name
  show (Vec _) = "vec"
  show (Vec2 False _) = "vec2"
  show (Vec2 True _) = "vec2'"

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
            | Vect2 Type Type Type Type Type Type
            | Concrete Type
  deriving (Eq, Ord, Show)

-- Type of expression with universally quantified variables
data Scheme = Scheme [TLabel] CType
  deriving (Eq, Ord)

instance Show Scheme where
  show (Scheme vs t) = concatMap (\name -> "forall " ++ name ++ ".") vs ++ show t
