
-- Backtracking Damas-Milner-style type inference engine (algorithm W)
-- Modified from https://github.com/wh5a/Algorithm-W-Step-By-Step

{-# LANGUAGE FlexibleInstances #-}

module Infer where

import Expr
import qualified Data.Set as Set
import Data.Set ((\\))
import qualified Data.Map as Map
import Data.List (nub)
import Control.Monad.State
import Control.Monad (when, guard)

-- Type substitution: map from type vars to types
type Sub = Map.Map TLabel Type

-- Empty substitution
nullSub :: Sub
nullSub = Map.empty

-- Composition of substitutions
composeSub :: Sub -> Sub -> Sub
composeSub s1 s2 = Map.map (applySub s2) s1 `Map.union` Map.map (applySub s1) s2

-- Things that have type vars and support substitution
class Types a where
  freeVars :: a -> Set.Set TLabel
  applySub :: Sub -> a -> a

instance Types Type where
  freeVars (TVar n)      = Set.singleton n
  freeVars (TFun t1 t2)  = freeVars t1 `Set.union` freeVars t2
  freeVars (TPair t1 t2) = freeVars t1 `Set.union` freeVars t2
  freeVars (TList t)     = freeVars t
  freeVars _             = Set.empty
  applySub s (TVar n)      = case Map.lookup n s of
                               Nothing -> TVar n
                               Just t  -> t
  applySub s (TFun t1 t2)  = TFun (applySub s t1) (applySub s t2)
  applySub s (TPair t1 t2) = TPair (applySub s t1) (applySub s t2)
  applySub s (TList t)     = TList $ applySub s t
  applySub _ t             = t

instance Types (TClass, Type) where
  freeVars (_, t) = freeVars t
  applySub s (con, t) = (con, applySub s t)

instance Types CType where
  freeVars (CType _ typ) = freeVars typ -- TODO: is this correct?
  applySub s (CType cons typ) = CType (applySub s cons) (applySub s typ)

instance Types Scheme where
  freeVars (Scheme vars t) = freeVars t \\ Set.fromList vars
  applySub s (Scheme vars t) = Scheme vars $ applySub (foldr Map.delete s vars) t

instance (Types a) => Types (Lit a) where
  freeVars (Lit _ typ) = freeVars typ
  applySub s (Lit name typ) = Lit name $ applySub s typ

instance (Types a) => Types (Exp (Lit a)) where
  freeVars _ = error "freeVars not implemented for expressions"
  applySub s e@(EVar _) = e
  applySub s (ELit l)     = ELit $ applySub s l
  applySub s (EApp f x)   = EApp (applySub s f) (applySub s x)
  applySub s (EAbs n e)   = EAbs n $ applySub s e
  applySub s (ELet n e b) = ELet n (applySub s e) (applySub s b)

instance (Types a) => Types [a] where
  freeVars l = foldr Set.union Set.empty $ map freeVars l
  applySub s = map $ applySub s

-- Type environment: types of expression variables
newtype TypeEnv = TypeEnv (Map.Map ELabel Scheme)

instance Types TypeEnv where
  freeVars (TypeEnv env) = freeVars $ Map.elems env
  applySub s (TypeEnv env) = TypeEnv $ Map.map (applySub s) env

-- Remove binding from environment
remove :: TypeEnv -> ELabel -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

-- Universally quantify all type variables in type that are not bound in environment
generalize :: TypeEnv -> CType -> Scheme
generalize env ct = Scheme vars ct
  where vars = Set.toList $ freeVars ct \\ freeVars env

-- State for generating unique type vars
data VarSupply = VarSupply {varSupply :: Int}

-- Monad for performing backtracking type inference
type Infer a = StateT VarSupply [] a

runInfer :: Infer a -> [(a, VarSupply)]
runInfer t = runStateT t initState
  where initState = VarSupply{varSupply = 0}

-- Generate a new type variable
newTVar :: String -> Infer Type
newTVar prefix = do
  s <- get
  put s{varSupply = varSupply s + 1}
  return $ TVar $ prefix ++ show (varSupply s)

-- Replace all bound variables with newly generated ones
instantiate :: Scheme -> Infer CType
instantiate (Scheme vars ct) = do
  newVars <- mapM (\_ -> newTVar "a") vars
  let s = Map.fromList $ zip vars newVars
  return $ applySub s ct

-- Bind a type variable to a type, return substitution
-- Fails if var occurs in the type (infinite type)
varBind :: TLabel -> Type -> Infer Sub
varBind name typ
  | TVar var <- typ, var == name   = return nullSub
  | name `Set.member` freeVars typ = fail ""
  | otherwise                      = return $ Map.singleton name typ

-- Most general unifier of two types
-- Returns substitution that makes them equal
-- Fails if types can't be unified
unify :: Type -> Type -> Infer Sub
unify (TFun arg1 res1) (TFun arg2 res2) =
  do argSub <- unify arg1 arg2
     resSub <- unify (applySub argSub res1) (applySub argSub res2)
     return $ argSub `composeSub` resSub
unify (TPair l1 r1) (TPair l2 r2) =
  do lSub <- unify l1 l2
     rSub <- unify (applySub lSub r1) (applySub lSub r2)
     return $ rSub `composeSub` rSub
unify (TList t1) (TList t2)        = unify t1 t2
unify (TVar name) typ              = varBind name typ
unify typ (TVar name)              = varBind name typ
unify (TConc a) (TConc b) | a == b = return nullSub
unify _ _                          = fail ""

-- Check typeclass constraints; remove those that hold, keep indeterminate ones, fail if any don't hold
checkCons :: [(TClass, Type)] -> Infer [(TClass, Type)]
checkCons [] = return []
checkCons (c:cs) = case holds c of
  Just cs' -> (cs' ++) <$> checkCons cs
  Nothing  -> fail ""

-- Infer type of literal
inferLit :: Lit Scheme -> Infer (Sub, CType, Exp (Lit CType))
inferLit lit@(Lit name typ) = do newTyp <- instantiate typ
                                 return (nullSub, newTyp, ELit $ Lit name newTyp)

-- Infer type of []-overloaded expression
-- All free expression variables must be bound in environment (otherwise it crashes)
-- Returns list of:
-- types of expression variables, type of whole expression, non-overloaded expression
infer :: TypeEnv -> Exp [Lit Scheme] -> Infer (Sub, CType, Exp (Lit CType))

-- Variable: find type in environment, combine constraints, return type
infer (TypeEnv env) (EVar name) = 
  case Map.lookup name env of
    Nothing    -> error $ "unbound variable: " ++ name
    Just sigma -> do typ <- instantiate sigma
                     return (nullSub, typ, EVar name)

-- Literal: apply helper function
-- This is the only source of nondeterminism (overloaded function literals)
infer _ (ELit lits) = do lit <- lift lits
                         inferLit lit

-- Lambda abstraction: add new unbound variable to environment, recurse to body, substitute back
infer env (EAbs name exp) =
  do newVar <- newTVar "b"
     let TypeEnv envMinusName = remove env name
         newEnv = TypeEnv $ Map.union envMinusName $ Map.singleton name $ Scheme [] $ CType [] newVar
     (sub, CType cons typ, newExp) <- infer newEnv exp
     return (sub, CType cons $ TFun (applySub sub newVar) typ, EAbs name (applySub sub newExp))

-- Application: infer function type and argument type, unify with function, check and reduce constraints
infer env exp@(EApp fun arg) =
  do newVar <- newTVar "c"
     (funSub, CType funCons funTyp, funExp) <- infer env fun
     (argSub, CType argCons argTyp, argExp) <- infer (applySub funSub env) arg
     uniSub <- unify (applySub argSub funTyp) (TFun argTyp newVar)
     let resSub = uniSub `composeSub` argSub `composeSub` funSub
     cons <- checkCons $ nub $ applySub resSub $ funCons ++ argCons
     return (resSub, CType cons $ applySub resSub newVar, EApp (applySub resSub funExp) (applySub resSub argExp))

-- Let binding: infer type of var from fix-enhanced exp, generalize to polytype, infer body, check and reduce constraints
infer env (ELet name exp body) =
    do let fixExp = EApp fixE $ EAbs name exp
       (expSub, expTyp@(CType expCons _), EApp _ (EAbs _ expExp)) <- infer env fixExp
       let TypeEnv envMinusName = remove env name
           expPoly = generalize (applySub expSub env) expTyp
           newEnv = TypeEnv $ Map.insert name expPoly envMinusName
       (bodySub, bodyTyp@(CType bodyCons _), bodyExp) <- infer (applySub expSub newEnv) body
       let resSub = expSub `composeSub` bodySub
       cons <- checkCons $ nub $ applySub resSub $ expCons ++ bodyCons
       return (resSub, bodyTyp, ELet name (applySub resSub expExp) (applySub resSub bodyExp))
         where fixE = ELit [Lit "fix" $ Scheme ["x"] $ CType [] $ TFun (TFun (TVar "x") (TVar "x")) (TVar "x")]

-- Main type inference function
typeInference :: Map.Map ELabel Scheme -> Exp [Lit Scheme] -> Infer (Sub, CType, Exp (Lit CType))
typeInference env exp =
  do (sub, typ, newExp) <- infer (TypeEnv env) exp
     return (sub, applySub sub typ, newExp)

-- Infer type under a constraint
inferType :: Bool -> Scheme -> Exp [Lit Scheme] -> [(CType, Exp (Lit CType))]
inferType constrainRes typeConstr exp = map fst $ runInfer $ do
  (infSub, CType infCons typ, infExp) <- typeInference Map.empty exp
  let subExp = applySub infSub infExp
  (resTyp, resExp) <-
    if constrainRes
    then do
      CType conCons genType <- instantiate typeConstr
      constrSub <- (composeSub infSub) <$> unify genType typ
      resCons <- checkCons $ nub $ applySub constrSub $ infCons ++ conCons
      defSub <- foldr defCons (return constrSub) resCons
      return (CType [] $ applySub defSub typ, applySub defSub subExp)
    else return (applySub infSub $ CType infCons typ, subExp)
  return (resTyp, resExp)
  where defCons (con, typ) msub = do
          sub <- msub
          instSub <- unify (defInst con) typ
          return $ instSub `composeSub` sub

-- TESTS

e0 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EVar "id")

e1 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EApp (EVar "id") (EVar "id"))

e2 = EApp
     (ELit [Lit "inc" $ Scheme [] $ CType [] $ TFun (TConc TInt) (TConc TInt),
            Lit "upper" $ Scheme [] $ CType [] $ TFun (TConc TChar) (TConc TChar)])
     (ELit [Lit "2" $ Scheme [] $ CType [] $ TConc TInt])

e3 = EApp
     (ELit [Lit "inc" $ Scheme [] $ CType [] $ TFun (TConc TInt) (TConc TInt),
            Lit "upper" $ Scheme [] $ CType [] $ TFun (TConc TChar) (TConc TChar)])
     (ELit [Lit "'a'" $ Scheme [] $ CType [] $ TConc TChar])

e4 = EApp
     (ELit [Lit "mapinc" $ Scheme [] $ CType [] $ TFun (TList (TConc TInt)) (TList (TConc TInt)),
            Lit "not" $ Scheme ["x"] $ CType [(Concrete, TVar "x")] $ TFun (TVar "x") (TConc TInt)])
     (ELit [Lit "[1]" $ Scheme [] $ CType [] $ TList (TConc TInt)])

e5 = EAbs "f" $
     ELet "x"
     (EApp (EVar "f") (EVar "x"))
     (EVar "x")

e6 = EApp
     (EApp
      (ELit [Lit "com" $ Scheme ["a","b","c"] $ CType [] $ (TVar "b" ~> TVar "c") ~> (TVar "a" ~> TVar "b") ~> (TVar "a" ~> TVar "c")])
      (ELit [Lit "consume" $ Scheme ["x"] $ CType [(Concrete, TVar "x")] $ TVar "x" ~> TConc TInt]))
     (ELit [Lit "produce" $ Scheme ["x"] $ CType [(Concrete, TVar "x")] $ TConc TInt ~> TVar "x"])
