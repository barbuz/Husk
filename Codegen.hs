
-- Code generation

module Codegen where

import Expr
import Infer

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Set as S (null)

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
consToHaskell :: TClass -> Maybe String
consToHaskell con | S.null $ freeVars con = Nothing
consToHaskell (Concrete t)                = Just $ "Concrete " ++ typeToHaskell t
consToHaskell (Vect _ _ _ _)              = Nothing
consToHaskell (Vect2 _ _ _ _ _ _)         = Nothing

-- Convert classed type to Haskell code
cTypeToHaskell :: CType -> String
cTypeToHaskell (CType cons typ)
  | cons' <- catMaybes $ map consToHaskell cons =
  if null cons'
  then typeToHaskell typ
  else "(" ++ intercalate "," cons' ++ ") => " ++ typeToHaskell typ

-- Convert expression to Haskell code
expToHaskell :: Exp (Lit CType) -> String
expToHaskell (EVar name) = name
expToHaskell (ELine n) = "line" ++ show n
expToHaskell (ELit (Value name typ)) = "(" ++ name ++ "::" ++ cTypeToHaskell typ ++ ")"
expToHaskell (ELit (Builtin name typ)) = "(func_" ++ name ++ "::" ++ cTypeToHaskell typ ++ ")"
expToHaskell (ELit (Vec typ)) = vecToHaskell typ
expToHaskell (ELit (Vec2 kind typ)) = vec2ToHaskell kind typ
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

-- Convert type of Vec2 to Haskell expression (nested zips)
-- Type will always be of the form (a -> b -> c) -> (x -> y -> z)
vec2ToHaskell kind typ@(CType _ (TFun (TFun a (TFun b c)) (TFun x (TFun y z)))) =
  "(" ++ nesting x y ++ "::" ++ cTypeToHaskell typ ++ ")"
  where nesting t1 t2 | t1 == a, t2 == b = "id"
                      | TList t1' <- t1, t2 == b = nesting t1' t2 ++ ".func_lmap"
                      | t1 == a, TList t2' <- t2 = nesting t1 t2' ++ ".func_rmap"
                      | TList t1' <- t1, TList t2' <- t2 = nesting t1' t2' ++ (if kind then ".func_zip'" else ".func_zip")
                      | otherwise = error $ "Illegal type for Vec2: " ++ show typ
