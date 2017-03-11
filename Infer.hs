
-- Backtracking Damas-Milner-style type inference engine (algorithm W)
-- Modified from https://github.com/wh5a/Algorithm-W-Step-By-Step

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
  freeVars (TVar n)     = Set.singleton n
  freeVars (TFun t1 t2) = freeVars t1 `Set.union` freeVars t2
  freeVars (TList t)    = freeVars t
  freeVars _            = Set.empty
  applySub s (TVar n)     = case Map.lookup n s of
                              Nothing -> TVar n
                              Just t  -> t
  applySub s (TFun t1 t2) = TFun (applySub s t1) (applySub s t2)
  applySub s (TList t)    = TList $ applySub s t
  applySub _ t            = t

instance Types TClass where
  freeVars (Concrete t) = freeVars t
  applySub s (Concrete t) = Concrete $ applySub s t

instance Types CType where
  freeVars (CType _ typ) = freeVars typ -- TODO: is this correct?
  applySub s (CType cons typ) = CType (applySub s cons) (applySub s typ)

instance Types Scheme where
  freeVars (Scheme vars t) = freeVars t \\ Set.fromList vars
  applySub s (Scheme vars t) = Scheme vars $ applySub (foldr Map.delete s vars) t

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
  | TVar var <- typ, var == name = return nullSub
  | name `Set.member` freeVars typ         = fail ""
  | otherwise                              = return $ Map.singleton name typ

-- Most general unifier of two types
-- Returns substitution that makes them equal
-- Fails if types can't be unified
unify :: Type -> Type -> Infer Sub
unify (TFun arg1 res1) (TFun arg2 res2) =
  do argSub <- unify arg1 arg2
     resSub <- unify (applySub argSub res1) (applySub argSub res2)
     return $ argSub `composeSub` resSub
unify (TList t1) (TList t2)        = unify t1 t2
unify (TVar name) typ              = varBind name typ
unify typ (TVar name)              = varBind name typ
unify (TConc a) (TConc b) | a == b = return nullSub
unify _ _                          = fail ""

-- Check typeclass constraints; remove those that hold, keep indeterminate ones, fail if any don't hold
checkCons :: [TClass] -> Infer [TClass]
checkCons [] = return []
checkCons (c:cs) = case holds c of
  Just True  -> checkCons cs
  Just False -> fail ""
  Nothing    -> (c :) <$> checkCons cs

-- Infer type of literal
inferLit :: Lit -> Infer (Sub, CType, Exp Lit)
inferLit lit@(Lit name typ) = do newTyp <- instantiate typ
                                 return (nullSub, newTyp, ELit lit)

-- Infer type of []-overloaded expression
-- All free expression variables must be bound in environment (otherwise it crashes)
-- Returns list of:
-- types of expression variables, type of whole expression, non-overloaded expression
infer :: TypeEnv -> Exp [Lit] -> Infer (Sub, CType, Exp Lit)

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
     return (sub, CType cons $ TFun (applySub sub newVar) typ, EAbs name newExp)

-- Application: infer function type and argument type, unify with function, check and reduce constraints
infer env exp@(EApp fun arg) =
  do newVar <- newTVar "c"
     (funSub, CType funCons funTyp, funExp) <- infer env fun
     (argSub, CType argCons argTyp, argExp) <- infer (applySub funSub env) arg
     uniSub <- unify (applySub argSub funTyp) (TFun argTyp newVar)
     let resSub = uniSub `composeSub` argSub `composeSub` funSub
     cons <- checkCons $ nub $ applySub resSub $ funCons ++ argCons
     return (resSub, CType cons $ applySub resSub newVar, EApp funExp argExp)

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
       return (resSub, bodyTyp, ELet name expExp bodyExp)
         where fixE = ELit [Lit "fix" $ Scheme ["x"] $ CType [] $ TFun (TFun (TVar "x") (TVar "x")) (TVar "x")]

-- Main type inference function
typeInference :: Map.Map ELabel Scheme -> Exp [Lit] -> Infer (Sub, CType, Exp Lit)
typeInference env exp =
  do (sub, typ, newExp) <- infer (TypeEnv env) exp
     return (sub, applySub sub typ, newExp)

-- Infer type under a constraint
inferType :: Bool -> Scheme -> Exp [Lit] -> [(CType, Exp Lit)]
inferType constrainRes typeConstr exp = map fst $ runInfer $ do
  (infSub, CType infCons typ, infExp) <- typeInference Map.empty exp
  CType conCons genType <- instantiate typeConstr
  constrSub <- unify genType typ
  resCons <- checkCons $ nub $ applySub (infSub `composeSub` constrSub) $ infCons ++ conCons
  when constrainRes $ guard $ null resCons
  return (CType resCons $ applySub constrSub typ, infExp)

-- TESTS

e0 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EVar "id")

e1 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EApp (EVar "id") (EVar "id"))

e2 = EApp
     (ELit [Lit "add" $ Scheme [] $ CType [] $ TFun (TConc TInt) (TConc TInt),
            Lit "or" $ Scheme [] $ CType [] $ TFun (TConc TBool) (TConc TBool)])
     (ELit [Lit "2" $ Scheme [] $ CType [] $ TConc TInt])

e3 = EApp
     (ELit [Lit "add" $ Scheme [] $ CType [] $ TFun (TConc TInt) (TConc TInt),
            Lit "or" $ Scheme [] $ CType [] $ TFun (TConc TBool) (TConc TBool)])
     (ELit [Lit "True" $ Scheme [] $ CType [] $ TConc TBool])

e4 = EApp
     (ELit [Lit "mapinc" $ Scheme [] $ CType [] $ TFun (TList (TConc TInt)) (TList (TConc TInt)),
            Lit "not" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $ TFun (TVar "x") (TConc TInt)])
     (ELit [Lit "[1]" $ Scheme [] $ CType [] $ TList (TConc TInt)])

e5 = EAbs "f" $
     ELet "x"
     (EApp (EVar "f") (EVar "x"))
     (EVar "x")
