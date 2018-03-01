
-- Backtracking Damas-Milner-style type inference engine (algorithm W)
-- Modified from https://github.com/wh5a/Algorithm-W-Step-By-Step

{-# LANGUAGE FlexibleInstances #-}

module Infer where

import Debug
import Expr
import qualified Data.Set as Set
import Data.Set ((\\))
import qualified Data.Map as Map
import Data.List (nub, unzip3)
import Control.Monad.State
import Control.Monad (when, guard, forM_)

-- Possible results for enforcing a typeclass
data Enforce = Enforce {otherCons :: [TClass],       -- "simpler" typeclass constraints
                        otherUnis :: [(Type, Type)]} -- types to be unified
  deriving (Show)

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

holds c@(Vect t1 t2 s1 s2)
  | s1 == t1, s2 == t2 = Just $ Enforce [] []
  | Nothing <- uniDepth t1 s1 = Nothing
  | Nothing <- uniDepth t2 s2 = Nothing
  | Just n <- eqDepth t1 s1 = Just $ Enforce [] [(iterate TList t2 !! n, s2)]
  | Just n <- eqDepth t2 s2 = Just $ Enforce [] [(iterate TList t1 !! n, s1)]
  | otherwise = Just $ Enforce [c] []

holds c@(Vect2 t1 t2 t3 s1 s2 s3)
  | TList _ <- t1 = Nothing
  | TList _ <- t2 = Nothing
  | TFun _ _ <- t1 = Nothing
  | TFun _ _ <- t2 = Nothing
  | TFun _ _ <- t3 = Nothing
  | TFun _ _ <- s1 = Nothing
  | TFun _ _ <- s2 = Nothing
  | TFun _ _ <- s3 = Nothing  -- Lists and functions are not bi-vectorizable for now
  | s1 == t1, s2 == t2, s3 == t3 = Just $ Enforce [] []
  | Nothing <- uniDepth t1 s1 = Nothing
  | Nothing <- uniDepth t2 s2 = Nothing
  | Nothing <- uniDepth t3 s3 = Nothing
  | Just n1 <- eqDepth t1 s1,
    Just n2 <- eqDepth t2 s2  = Just $ Enforce [] [(iterate TList t3 !! max n1 n2, s3)]
  | Just n1 <- eqDepth t1 s1,
    Just n3 <- eqDepth t3 s3,
    n1 < n3                   = Just $ Enforce [] [(iterate TList t2 !! n3, s2)]
  | Just n2 <- eqDepth t2 s2,
    Just n3 <- eqDepth t3 s3,
    n2 < n3                   = Just $ Enforce [] [(iterate TList t1 !! n3, s1)]
  | otherwise = Just $ Enforce [c] []

-- Default typeclass instances, given as unifiable pairs of types
-- The choice is nondeterministic, which is modeled by a list of possibilities
defInst :: TClass -> [[(Type, Type)]]
defInst (Concrete t)       = [[(t, TConc TNum)]]
defInst (Vect t1 t2 s1 s2) = [[(s1, iterate TList t1 !! max n1 n2)
                              ,(s2, iterate TList t2 !! max n1 n2)]]
  where Just n1 = uniDepth t1 s1
        Just n2 = uniDepth t2 s2
defInst (Vect2 t1 t2 t3 s1 s2 s3) = [ [(s1, iterate TList t1 !! k1)
                                      ,(s2, iterate TList t2 !! k2)
                                      ,(s3, iterate TList t3 !! max k1 k2)]
                                    | k1 <- [maxN, maxN-1 .. n1]
                                    , k2 <- [maxN, maxN-1 .. n2]]
  where Just n1 = uniDepth t1 s1
        Just n2 = uniDepth t2 s2
        Just n3 = uniDepth t3 s3
        maxN    = maximum [n1, n2, n3]

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
  freeVars (Vect t1 t2 s1 s2) = freeVars [t1,t2,s1,s2]
  freeVars (Vect2 t1 t2 t3 s1 s2 s3) = freeVars [t1,t2,t3,s1,s2,s3]
  applySub s (Concrete t)       = Concrete $ applySub s t
  applySub s (Vect t1 t2 s1 s2) = Vect (applySub s t1) (applySub s t2) (applySub s s1) (applySub s s2)
  applySub s (Vect2 t1 t2 t3 s1 s2 s3) =
    Vect2 (applySub s t1) (applySub s t2) (applySub s t3) (applySub s s1) (applySub s s2) (applySub s s3)

instance Types CType where
  freeVars (CType _ typ) = freeVars typ -- TODO: is this correct?
  applySub s (CType cons typ) = CType (applySub s cons) (applySub s typ)

instance Types Scheme where
  freeVars (Scheme vars t) = freeVars t \\ Set.fromList vars
  applySub s (Scheme vars t) = Scheme vars $ applySub (foldr Map.delete s vars) t

instance (Types a) => Types (Lit a) where
  freeVars (Value _ typ) = freeVars typ
  freeVars (Builtin _ typ) = freeVars typ
  freeVars (Vec typ) = freeVars typ
  freeVars (Vec2 _ typ) = freeVars typ
  applySub s (Value name typ) = Value name $ applySub s typ
  applySub s (Builtin name typ) = Builtin name $ applySub s typ
  applySub s (Vec typ) = Vec $ applySub s typ
  applySub s (Vec2 kind typ) = Vec2 kind $ applySub s typ

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

-- Run a monadic computation with Infer, using given set of lines
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
  return $ trace' 2 ("substituting " ++ show t ++ " with " ++ show (Map.toList sub)) $ applySub sub t

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
  | name `Set.member` freeVars typ = trace' 2 "occurs check fail" $ fail ""
  | otherwise                      = updateSub $ Map.singleton name typ

-- Most general unifier of two types
-- Updates substitution in a way that makes them equal
-- Fails if types can't be unified
unify :: Type -> Type -> Infer ()
unify t1 t2 | trace' 2 ("unifying " ++ show t1 ++ " and " ++ show t2) False = undefined
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
    unify' _ _                          = trace' 2 "unification fail" $ fail ""

-- Check typeclass constraints; remove those that hold, keep indeterminate ones, perform unifications, fail if any don't hold
checkCons :: [TClass] -> Infer [TClass]
checkCons (x:_) | trace' 2 ("checking " ++ show x) False = undefined
checkCons [] = return []
checkCons (c:cs) = case traceShow' 2 (c, holds c) $ holds c of
  Just (Enforce newCs unis) -> do
    mapM (uncurry unify) unis
    (newCs ++) <$> checkCons cs
  Nothing  -> trace' 2 "constraint fail" $ fail ""

-- Infer type of literal
inferLit :: Lit Scheme -> Infer (CType, Exp (Lit CType))
inferLit x | trace' 2 ("chose " ++ show x) False = undefined
inferLit lit@(Value name typ) =
  do newTyp <- instantiate typ
     return (newTyp, ELit $ Value name newTyp)
inferLit lit@(Builtin name typ) =
  do newTyp <- instantiate typ
     return (newTyp, ELit $ Builtin name newTyp)
inferLit lit@(Vec typ) =
  do newTyp <- instantiate typ
     return (newTyp, ELit $ Vec newTyp)
inferLit lit@(Vec2 kind typ) =
  do newTyp <- instantiate typ
     return (newTyp, ELit $ Vec2 kind newTyp)

-- Infer type of []-overloaded expression
-- All free expression variables must be bound in environment (otherwise it crashes)
-- Returns tuple of:
-- type of whole expression, non-overloaded expression
-- Second argument is type hint
infer :: TypeEnv -> Maybe Type -> Exp [Lit Scheme] -> Infer (CType, Exp (Lit CType))
infer env _ exp | trace' 2 ("inferring " ++ show exp) False = undefined

-- Variable: find type in environment, combine constraints, return type
infer (TypeEnv env) _ (EVar name) = 
  case Map.lookup name env of
    Nothing    -> error $ "unbound variable: " ++ name
    Just sigma -> do typ <- instantiate sigma
                     return (typ, EVar name)

-- Line reference: pull type if already inferred, otherwise infer it
infer env hint (ELine num) =
  do lineExpr <- gets $ (Map.! num) . lineExprs
     case lineExpr of
       Unprocessed expr -> do
         initTyp <- case hint of
                      Just typ -> return typ
                      Nothing  -> newTVar "l"
         updateLines $ Map.insert num $ Processing initTyp
         (typ@(CType _ resTyp), infExpr) <- infer env hint expr
         unify initTyp resTyp
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
infer _ hint (ELit lits) = do lit <- lift lits
                              res@(CType _ typ, _) <- inferLit lit
                              case hint of
                                Just hintTyp -> unify typ hintTyp
                                _            -> return ()
                              return res

-- Lambda abstraction: add new unbound variable to environment, recurse to body, substitute back
infer env hint (EAbs name exp) =
  do newVar <- case hint of
                 Just (TFun arg _) -> return arg
                 _                 -> newTVar "b"
     let TypeEnv envMinusName = remove env name
         newEnv = TypeEnv $ Map.union envMinusName $ Map.singleton name $ Scheme [] $ CType [] newVar
     (CType cons typ, newExp) <- case hint of
       Just (TFun _ res) -> infer newEnv (Just res) exp
       _                 -> infer newEnv Nothing exp
     varTyp <- substitute newVar
     retExp <- substitute newExp
     return (CType cons $ TFun varTyp typ, EAbs name retExp)

-- Application: infer function type and argument type, unify with function, check and reduce constraints
infer env hint exp@(EApp fun arg) = --traceShow' (fun,arg) $
  do newVar <- case hint of
                 Just typ -> return typ
                 Nothing  -> newTVar "c"
     (CType funCons funTyp, funExp) <- infer env Nothing fun
     newEnv <- substitute env
     (CType argCons argTyp, argExp) <- case funTyp of
       TFun funcArg funcRes -> do
         unify funcRes newVar
         infer newEnv (Just  funcArg) arg
       _                    -> do
         infer newEnv Nothing arg
     unify funTyp (TFun argTyp newVar)
     cons <- checkCons . nub =<< substitute (funCons ++ argCons)
     varTyp <- substitute newVar
     newFunExp <- substitute funExp
     newArgExp <- substitute argExp
     return (CType cons varTyp, EApp newFunExp newArgExp)

-- Infix operator: infer as binary function, but in order first arg -> second arg -> operator
-- If second arg is lambda or line reference, order is first -> operator -> second to take advantage of hints
-- Replace with two function applications in result
infer env hint exp@(EOp op argL argR) = do
  newVar <- case hint of
              Just typ -> return typ
              Nothing  -> newTVar "c"
  (CType lCons lTyp, lExp) <- infer env Nothing argL
  newEnv <- substitute env
  (CType rCons rTyp, rExp) <- infer newEnv Nothing argR
  newEnv2 <- substitute newEnv
  (CType opCons opTyp, opExp) <- let opHint = Just $ TFun lTyp $ TFun rTyp newVar
                                 in infer newEnv2 opHint op
  unify opTyp (TFun lTyp $ TFun rTyp newVar)
  varTyp <- substitute newVar
  cons <- checkCons . nub =<< substitute (opCons ++ rCons ++ lCons)
  [newOpExp, newLExp, newRExp] <- mapM substitute [opExp, lExp, rExp]
  return (CType cons varTyp, EApp (EApp newOpExp newLExp) newRExp)

-- Let binding: infer type of var from fix-enhanced exp, generalize to polytype, infer body, check and reduce constraints
infer env _ (ELet name exp body) =
    do let fixExp = EApp fixE $ EAbs name exp
       (expTyp@(CType expCons _), EApp _ (EAbs _ expExp)) <- infer env Nothing fixExp
       subEnv <- substitute env
       let TypeEnv envMinusName = remove env name
           expPoly = generalize subEnv expTyp
       newEnv <- substitute $ TypeEnv $ Map.insert name expPoly envMinusName
       (bodyTyp@(CType bodyCons _), bodyExp) <- infer newEnv Nothing body
       cons <- checkCons . nub <$> substitute (expCons ++ bodyCons)
       newExpExp <- substitute expExp
       newBodyExp <- substitute bodyExp
       return (bodyTyp, ELet name newExpExp newBodyExp)
         where fixE = ELit [Builtin "fix" $ Scheme ["x"] $ CType [] $ TFun (TFun (TVar "x") (TVar "x")) (TVar "x")]

-- Main type inference function, with a hint about the resulting type
typeInference :: Map.Map ELabel Scheme -> Scheme -> Exp [Lit Scheme] -> Infer CType
typeInference env hint expr =
  do CType _ hintTyp <- instantiate hint
     (typ, newExp) <- infer (TypeEnv env) (Just hintTyp) expr
     newTyp <- substitute typ
     return newTyp

-- Prune admissible types of builtins based on local patterns
prune :: Exp [Lit Scheme] -> Exp [Lit Scheme]
prune (EOp (ELit ops) (ELit largs) (ELit rargs)) = EOp (selectLits newOps ops) (selectLits newLargs largs) (selectLits newRargs rargs)
  where (newOps, newLargs, newRargs) =
          unzip3 [(op, larg, rarg) | op <- ops, larg <- largs, rarg <- rargs,
                              not . null . inferSimple $ EOp (ELit [op]) (ELit [larg]) (ELit [rarg])]
prune (EOp (ELit ops) larg (ELit rargs)) = EOp (selectLits newOps ops) (prune larg) (selectLits newRargs rargs)
  where (newOps, newRargs) =
          unzip [(op, rarg) | op <- ops, rarg <- rargs,
                              not . null . inferSimple $ EOp (ELit [op]) undef (ELit [rarg])]
        undef = ELit [Value "undef" $ Scheme ["x"] $ CType [] $ TVar "x"]
prune (EOp op larg rarg) = EOp op (prune larg) (prune rarg)
prune (EApp larg rarg) = EApp (prune larg) (prune rarg)
prune (EAbs var expr) = EAbs var $ prune expr
prune (ELet var expr body) = ELet var (prune expr) (prune body)
prune expr = expr

selectLits :: [Lit Scheme] -> [Lit Scheme] -> Exp [Lit Scheme]
selectLits news olds = ELit $ filter (`elem` news) olds

-- Infer types of a single expression out of context
inferSimple :: Exp [Lit Scheme] -> [(CType, InfState)]
inferSimple expr = runInfer [] $ typeInference Map.empty (Scheme ["x"] $ CType [] $ TVar "x") expr

-- Infer types of lines under a constraint
inferType :: Bool -> Scheme -> [Exp [Lit Scheme]] -> [[(Int, CType, Exp (Lit CType))]]
inferType constrainRes typeConstr exprs = trace' 1 ("inferring program " ++ show pruned) $ map fst $ runInfer pruned $ do
  CType infCons typ <- typeInference Map.empty typeConstr (ELine 0)
  when constrainRes $ do
    CType conCons genType <- instantiate typeConstr
    trace' 1 "applying constraints" $ unify genType typ
    trace' 1 "defaulting instances" $ forM_ (nub $ infCons ++ conCons) $ \con -> do
      newCons <- checkCons =<< substitute [con]
      forM_ newCons $ \newCon -> do
        insts <- lift $ defInst newCon
        mapM_ (uncurry unify) insts
  lExprs <- Map.assocs <$> gets lineExprs
  flip mapM [(i, exp, typ) | (i, Processed exp typ) <- lExprs] $
    \(i, exp, typ) -> do
      newExp <- substitute exp
      newTyp <- substitute typ
      return (i, newTyp, newExp)
  where pruned = trace' 1 ("pruning program " ++ show exprs) $ prune <$> exprs

-- TESTS

e0 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EVar "id")

e1 = ELet "id"
     (EAbs "x" (EVar "x"))
     (EApp (EVar "id") (EVar "id"))

e2 = EApp
     (ELit [Builtin "inc" $ Scheme [] $ CType [] $ TFun (TConc TNum) (TConc TNum),
            Builtin "upper" $ Scheme [] $ CType [] $ TFun (TConc TChar) (TConc TChar)])
     (ELit [Value "2" $ Scheme [] $ CType [] $ TConc TNum])

e3 = EApp
     (ELit [Builtin "inc" $ Scheme [] $ CType [] $ TFun (TConc TNum) (TConc TNum),
            Builtin "upper" $ Scheme [] $ CType [] $ TFun (TConc TChar) (TConc TChar)])
     (ELit [Value "'a'" $ Scheme [] $ CType [] $ TConc TChar])

e4 = EApp
     (ELit [Builtin "mapinc" $ Scheme [] $ CType [] $ TFun (TList (TConc TNum)) (TList (TConc TNum)),
            Builtin "not" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $ TFun (TVar "x") (TConc TNum)])
     (ELit [Value "[1]" $ Scheme [] $ CType [] $ TList (TConc TNum)])

e5 = EAbs "f" $
     ELet "x"
     (EApp (EVar "f") (EVar "x"))
     (EVar "x")

e6 = EApp
     (EApp
      (ELit [Builtin "com" $ Scheme ["a","b","c"] $ CType [] $ (TVar "b" ~> TVar "c") ~> (TVar "a" ~> TVar "b") ~> (TVar "a" ~> TVar "c")])
      (ELit [Builtin "consume" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $ TVar "x" ~> TConc TNum]))
     (ELit [Builtin "produce" $ Scheme ["x"] $ CType [Concrete (TVar "x")] $ TConc TNum ~> TVar "x"])

e7 = EApp
     (ELit [Builtin "vecneg" $ Scheme ["a","b"] $ CType [Vect (TConc TNum) (TConc TNum) (TVar "a") (TVar "b")] $ TVar "a" ~> TVar "b"])
     (ELit [Value "[[[1]]]" $ Scheme [] $ CType [] $ TList (TList (TList (TConc TNum)))])
