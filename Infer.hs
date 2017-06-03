
-- Backtracking Damas-Milner-style type inference engine (algorithm W)
-- Modified from https://github.com/wh5a/Algorithm-W-Step-By-Step

{-# LANGUAGE FlexibleInstances #-}

module Infer where

import Debug
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

instance Types TClass where
  freeVars (Concrete t)       = freeVars t
  freeVars (Number t)         = freeVars t
  freeVars (Vect t1 t2 s1 s2) = freeVars [t1,t2,s1,s2]
  applySub s (Concrete t)       = Concrete $ applySub s t
  applySub s (Number t)         = Number $ applySub s t
  applySub s (Vect t1 t2 s1 s2) = Vect (applySub s t1) (applySub s t2) (applySub s s1) (applySub s s2)

instance Types CType where
  freeVars (CType _ typ) = freeVars typ -- TODO: is this correct?
  applySub s (CType cons typ) = CType (applySub s cons) (applySub s typ)

instance Types Scheme where
  freeVars (Scheme vars t) = freeVars t \\ Set.fromList vars
  applySub s (Scheme vars t) = Scheme vars $ applySub (foldr Map.delete s vars) t

instance (Types a) => Types (Lit a) where
  freeVars (Lit _ _ typ) = freeVars typ
  applySub s (Lit prefix name typ) = Lit prefix name $ applySub s typ

instance (Types a) => Types (Exp (Lit a)) where
  freeVars _ = error "freeVars not implemented for expressions"
  applySub s = fmap $ applySub s

instance (Types a) => Types [a] where
  freeVars l = foldr Set.union Set.empty $ map freeVars l
  applySub s = map $ applySub s

-- Type environment: types of expression variables
newtype TypeEnv = TypeEnv (Map.Map ELabel Scheme)
  deriving (Show)

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

-- Type for line functions with possibly inferred type
data LineFunc = Unprocessed (Exp [Lit Scheme])
              | Processing Type
              | Processed (Exp (Lit CType)) CType
              deriving (Show)

-- State for generating unique type vars
data InfState = InfState {varSupply :: Int,
                          currSubst :: Sub,
                          lineExprs :: Map.Map Int LineFunc}

-- Monad for performing backtracking type inference
type Infer a = StateT InfState [] a

runInfer :: [Exp [Lit Scheme]] -> Infer a -> [(a, InfState)]
runInfer exps t = runStateT t initState
  where initState = InfState {varSupply = 0,
                              currSubst = nullSub,
                              lineExprs = Map.fromList [(i, Unprocessed e) | (i, e) <- zip [0..] exps]}

-- Generate a new type variable
newTVar :: String -> Infer Type
newTVar prefix = do
  s <- get
  put s{varSupply = varSupply s + 1}
  return $ TVar $ prefix ++ show (varSupply s)

-- Update current substitution with new one
updateSub :: Sub -> Infer ()
updateSub sub = do
  s <- get
  put s{currSubst = currSubst s `composeSub` sub}

-- Update line functions
updateLines :: (Map.Map Int LineFunc -> Map.Map Int LineFunc) -> Infer ()
updateLines f = do
  s <- get
  put s{lineExprs = f $ lineExprs s}

-- Apply current substitution
substitute :: (Show t, Types t) => t -> Infer t
substitute t = do
  sub <- gets currSubst
  return $ trace' ("substituting " ++ show t ++ " with " ++ show (Map.toList sub)) $ applySub sub t

-- Replace all bound variables with newly generated ones
instantiate :: Scheme -> Infer CType
instantiate (Scheme vars ct) = do
  newVars <- mapM (\_ -> newTVar "a") vars
  let s = Map.fromList $ zip vars newVars
  return $ applySub s ct

-- Bind a type variable to a type, update current substitution
-- Fails if var occurs in the type (infinite type)
varBind :: TLabel -> Type -> Infer ()
varBind name typ
  | TVar var <- typ, var == name   = return ()
  | name `Set.member` freeVars typ = fail ""
  | otherwise                      = updateSub $ Map.singleton name typ

-- Most general unifier of two types
-- Updates substitution in a way that makes them equal
-- Fails if types can't be unified
unify :: Type -> Type -> Infer ()
unify t1 t2 | trace' ("unifying " ++ show (t1, t2)) False = undefined
unify t1 t2 = do
  t1' <- substitute t1
  t2' <- substitute t2
  unify' t1' t2'
  where
    unify' (TFun arg1 res1) (TFun arg2 res2) =
      do unify' arg1 arg2
         unify res1 res2
    unify' (TPair l1 r1) (TPair l2 r2) =
      do unify' l1 l2
         unify r1 r2
    unify' (TList t1) (TList t2)        = unify' t1 t2
    unify' (TVar name) typ              = varBind name typ
    unify' typ (TVar name)              = varBind name typ
    unify' (TConc a) (TConc b) | a == b = return ()
    unify' _ _                          = fail ""

-- Check typeclass constraints; remove those that hold, keep indeterminate ones, fail if any don't hold
checkCons :: [TClass] -> Infer [TClass]
checkCons x | trace' ("checking " ++ show x) False = undefined
checkCons [] = return []
checkCons (c:cs) = case {-traceShow' (c, holds c)-} holds c of
  Just cs' -> (cs' ++) <$> checkCons cs
  Nothing  -> fail ""

-- Infer type of literal
inferLit :: Lit Scheme -> Infer (CType, Exp (Lit CType))
inferLit lit@(Lit prefix name typ) =
  do newTyp <- instantiate typ
     return (newTyp, ELit $ Lit prefix name newTyp)

-- Infer type of []-overloaded expression
-- All free expression variables must be bound in environment (otherwise it crashes)
-- Returns tuple of:
-- type of whole expression, non-overloaded expression
infer :: TypeEnv -> Exp [Lit Scheme] -> Infer (CType, Exp (Lit CType))
infer env exp | trace' ("inferring " ++ show exp) False = undefined

-- Variable: find type in environment, combine constraints, return type
infer (TypeEnv env) (EVar name) = 
  case Map.lookup name env of
    Nothing    -> error $ "unbound variable: " ++ name
    Just sigma -> do typ <- instantiate sigma
                     return (typ, EVar name)

-- Line reference: pull type if already inferred, otherwise infer it
infer env (ELine num) =
  do lineExpr <- gets $ (Map.! num) . lineExprs
     case lineExpr of
       Unprocessed expr -> do
         newVar <- newTVar "l"
         updateLines $ Map.insert num $ Processing newVar
         (typ@(CType _ t), infExpr) <- infer env expr
         unify newVar t
         newExpr <- substitute infExpr
         newTyp <- substitute typ
         updateLines $ Map.insert num $ Processed newExpr newTyp
         return (newTyp, ELine num)
       Processing typ -> do
         newTyp <- substitute typ
         updateLines $ Map.insert num $ Processing newTyp
         return (CType [] $ newTyp, ELine num)
       Processed exp typ -> do
         newTyp <- substitute typ
         newExp <- substitute exp
         updateLines $ Map.insert num $ Processed newExp newTyp
         return (newTyp, ELine num)
         

-- Literal: apply helper function
-- This is the only source of nondeterminism (overloaded function literals)
infer _ (ELit lits) = do lit <- lift lits
                         inferLit lit

-- Lambda abstraction: add new unbound variable to environment, recurse to body, substitute back
infer env (EAbs name exp) =
  do newVar <- newTVar "b"
     let TypeEnv envMinusName = remove env name
         newEnv = TypeEnv $ Map.union envMinusName $ Map.singleton name $ Scheme [] $ CType [] newVar
     (CType cons typ, newExp) <- infer newEnv exp
     varTyp <- substitute newVar
     retExp <- substitute newExp
     return (CType cons $ TFun varTyp typ, EAbs name retExp)

-- Application: infer function type and argument type, unify with function, check and reduce constraints
infer env exp@(EApp fun arg) = --traceShow' (fun,arg) $
  do newVar <- newTVar "c"
     (CType funCons funTyp, funExp) <- infer env fun
     newEnv <- substitute env
     (CType argCons argTyp, argExp) <- infer newEnv arg
     newFunTyp <- substitute funTyp
     unify newFunTyp (TFun argTyp newVar)
     cons <- checkCons . nub =<< substitute (funCons ++ argCons)
     varTyp <- substitute newVar
     newFunExp <- substitute funExp
     newArgExp <- substitute argExp
     return (CType cons varTyp, EApp newFunExp newArgExp)

-- Infix operator: infer as binary function, but in order first arg -> second arg -> operator
-- Replace with two function applications in result
infer env exp@(EOp op argL argR) = --traceShow' (op, argL, argR) $
  do newVar <- newTVar "c"
     (CType lCons lTyp, lExp) <- infer env argL
     newEnv <- substitute env
     (CType rCons rTyp, rExp) <- infer newEnv argR
     newEnv2 <- substitute newEnv
     (CType opCons opTyp, opExp) <- infer newEnv2 op
     unify opTyp (TFun lTyp $ TFun rTyp newVar)
     varTyp <- substitute newVar
     cons <- checkCons . nub =<< substitute (opCons ++ rCons ++ lCons)
     [newOpExp, newLExp, newRExp] <- mapM substitute [opExp, lExp, rExp]
     return (CType cons varTyp, EApp (EApp newOpExp newLExp) newRExp)

-- Let binding: infer type of var from fix-enhanced exp, generalize to polytype, infer body, check and reduce constraints
infer env (ELet name exp body) =
    do let fixExp = EApp fixE $ EAbs name exp
       (expTyp@(CType expCons _), EApp _ (EAbs _ expExp)) <- infer env fixExp
       subEnv <- substitute env
       let TypeEnv envMinusName = remove env name
           expPoly = generalize subEnv expTyp
       newEnv <- substitute $ TypeEnv $ Map.insert name expPoly envMinusName
       (bodyTyp@(CType bodyCons _), bodyExp) <- infer newEnv body
       cons <- checkCons . nub <$> substitute (expCons ++ bodyCons)
       newExpExp <- substitute expExp
       newBodyExp <- substitute bodyExp
       return (bodyTyp, ELet name newExpExp newBodyExp)
         where fixE = ELit [Lit "func_" "fix" $ Scheme ["x"] $ CType [] $ TFun (TFun (TVar "x") (TVar "x")) (TVar "x")]

-- Main type inference function
typeInference :: Map.Map ELabel Scheme -> Exp [Lit Scheme] -> Infer CType
typeInference env expr =
  do (typ, newExp) <- infer (TypeEnv env) expr
     newTyp <- substitute typ
     return newTyp

-- Infer types of lines under a constraint
inferType :: Bool -> Scheme -> [Exp [Lit Scheme]] -> [[(Int, CType, Exp (Lit CType))]]
inferType constrainRes typeConstr exprs = trace' ("inferring program " ++ show exprs) $ map fst $ runInfer exprs $ do
  CType infCons typ <- typeInference Map.empty $ ELine 0
  when constrainRes $ do
    CType conCons genType <- instantiate typeConstr
    unify genType typ
    resCons <- checkCons . nub =<< substitute (infCons ++ conCons)
    flip mapM_ resCons $ \con -> do
      let (t1, t2) = defInst con
      unify t1 t2
  lExprs <- Map.assocs <$> gets lineExprs
  flip mapM [(i, exp, typ) | (i, Processed exp typ) <- lExprs] $
    \(i, exp, typ) -> do
      newExp <- substitute exp
      newTyp <- substitute typ
      return (i, newTyp, newExp)

-- TESTS

e0 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EVar "id")

e1 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EApp (EVar "id") (EVar "id"))

e2 = EApp
     (ELit [Lit "" "inc" $ Scheme [] $ CType [] $ TFun (TConc TInt) (TConc TInt),
            Lit "" "upper" $ Scheme [] $ CType [] $ TFun (TConc TChar) (TConc TChar)])
     (ELit [Lit "" "2" $ Scheme [] $ CType [] $ TConc TInt])

e3 = EApp
     (ELit [Lit "" "inc" $ Scheme [] $ CType [] $ TFun (TConc TInt) (TConc TInt),
            Lit "" "upper" $ Scheme [] $ CType [] $ TFun (TConc TChar) (TConc TChar)])
     (ELit [Lit "" "'a'" $ Scheme [] $ CType [] $ TConc TChar])

e4 = EApp
     (ELit [Lit "" "mapinc" $ Scheme [] $ CType [] $ TFun (TList (TConc TInt)) (TList (TConc TInt)),
            Lit "" "not" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $ TFun (TVar "x") (TConc TInt)])
     (ELit [Lit "" "[1]" $ Scheme [] $ CType [] $ TList (TConc TInt)])

e5 = EAbs "f" $
     ELet "x"
     (EApp (EVar "f") (EVar "x"))
     (EVar "x")

e6 = EApp
     (EApp
      (ELit [Lit "" "com" $ Scheme ["a","b","c"] $ CType [] $ (TVar "b" ~> TVar "c") ~> (TVar "a" ~> TVar "b") ~> (TVar "a" ~> TVar "c")])
      (ELit [Lit "" "consume" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $ TVar "x" ~> TConc TInt]))
     (ELit [Lit "" "produce" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $ TConc TInt ~> TVar "x"])

e7 = EApp
     (ELit [Lit "" "vecneg" $ Scheme ["a","b"] $ CType [Vect (TConc TInt) (TConc TInt) (TVar "a") (TVar "b")] $ TVar "a" ~> TVar "b"])
     (ELit [Lit "" "[[[1]]]" $ Scheme [] $ CType [] $ TList (TList (TList (TConc TInt)))])
