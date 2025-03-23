module Bruijn where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (chr)
import Data.List (nub, union, (\\))
import qualified Data.Map as Map

data Expr a where
  Var :: Int -> Expr a
  Int :: Int -> Expr Int
  Abs :: Expr b -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b
  Let :: Expr a -> Expr b -> Expr b
  Bop :: BopE -> Expr Int -> Expr Int -> Expr Int
  Op :: OpE -> Expr a -> Expr a
  IfZ :: Expr Int -> Expr a -> Expr a -> Expr a
  FixP :: Expr (a -> a) -> Expr a
  Tup :: Expr a -> Expr b -> Expr (a, b)

-- variants
-- Variant :: (String, Expr a) -> Expr a
-- Match :: (Expr a, list (String, Int, Expr a)) -> Expr a

data BopE
  = Add
  | Mul
  | Sub

bopToOp :: BopE -> (Int -> Int -> Int)
bopToOp op = case op of
  Add -> (+)
  Mul -> (*)
  Sub -> (-)

data OpE
  = Fst
  | Snd

instance Show (Expr Int) where
  show = aux
   where
    aux :: Expr a -> String
    aux t =
      case t of
        Int x -> show x
        Var idx -> "(VAR: " ++ show idx ++ ")"
        Abs e -> "(\\_ -> " ++ aux e ++ ")"
        App e1 e2 -> "(" ++ aux e1 ++ " " ++ aux e2 ++ ")"
        Let e1 e2 -> "let _ = " ++ aux e1 ++ " in " ++ aux e2
        Bop op e1 e2 -> "(" ++ aux e1 ++ show op ++ aux e2 ++ ")"
        Op op e -> "(" ++ show op ++ aux e ++ ")"
        IfZ e1 e2 e3 -> "if " ++ aux e1 ++ " is 0 then " ++ aux e2 ++ " else " ++ aux e3
        FixP e -> "fix of " ++ aux e
        Tup a b -> "(" ++ aux a ++ ", " ++ aux b ++ ")"

instance Show BopE where
  show Add = " + "
  show Mul = " * "
  show Sub = " - "

instance Show OpE where
  show Fst = "fst "
  show Snd = "snd "

data Type
  = TInt
  | TVar Int
  | TArrow Type Type
  | TTuple Type Type
  deriving (Eq)

instance Show Type where
  show t = case t of
    TInt -> "Int"
    TVar n -> "'" ++ show (chr n)
    TArrow l r -> show l ++ " -> " ++ show r
    TTuple l r -> "(" ++ show l ++ ", " ++ show r ++ ")"

data Scheme = Forall [Int] Type

type Subst = Map.Map Int Type

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Types a where
  freeVars :: a -> [Int]
  apply :: Subst -> a -> a

instance Types Type where
  freeVars TInt = []
  freeVars (TVar n) = [n]
  freeVars (TArrow t1 t2) = freeVars t1 `union` freeVars t2
  freeVars (TTuple t1 t2) = freeVars t1 `union` freeVars t2

  apply s TInt = TInt
  apply s t@(TVar n) = Map.findWithDefault t n s
  apply s (TArrow t1 t2) = TArrow (apply s t1) (apply s t2)
  apply s (TTuple t1 t2) = TTuple (apply s t1) (apply s t2)

instance Types Scheme where
  freeVars (Forall vars t) = freeVars t \\ vars
  apply s (Forall vars t) = Forall vars (apply (foldr Map.delete s vars) t)

applyEnv :: Subst -> TEnv -> TEnv
applyEnv s = Map.map (apply s)

type TEnv = Map.Map Int Scheme

freeVarsEnv :: TEnv -> [Int]
freeVarsEnv env = nub $ concatMap freeVars (Map.elems env)

-- Inference Monad and Fresh Variable Generation
type Infer a = ExceptT String (State Int) a

newTyVar :: Infer Type
newTyVar = do
  n <- get
  modify (+ 1)
  return (TVar n)
generalize :: TEnv -> Type -> Scheme
generalize env t = Forall vars t
 where
  vars = freeVars t \\ freeVarsEnv env

-- unification of type variables
unify :: Type -> Type -> Infer Subst
unify TInt TInt = return Map.empty
unify (TVar n) t = varBind n t
unify t (TVar n) = varBind n t
unify (TArrow t1 t2) (TArrow t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return (s2 `composeSubst` s1)
unify (TTuple t1 t2) (TTuple t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return (s2 `composeSubst` s1)
unify t1 t2 = throwError $ "Types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: Int -> Type -> Infer Subst
varBind n t
  | t == TVar n = return Map.empty
  | n `elem` freeVars t = throwError $ "Occurs check fails: " ++ show n ++ " vs. " ++ show t
  | otherwise = return (Map.singleton n t)

infer :: TEnv -> Expr a -> Infer (Subst, Type)
infer env (Var n) =
  case Map.lookup n env of
    Nothing -> throwError $ "Unbound variable: " ++ show n
    Just sigma -> do
      t <- instantiate sigma
      return (Map.empty, t)
infer env (Int _) =
  return (Map.empty, TInt)
infer env (Abs e) = do
  tv <- newTyVar
  -- In a de Bruijn setting we extend the environment by “pushing” a new binding.
  let env' = Map.insert 0 (Forall [] tv) (shiftEnv 1 env)
  (s1, tBody) <- infer env' e
  return (s1, TArrow (apply s1 tv) tBody)
infer env (App e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applyEnv s1 env) e2
  tv <- newTyVar
  s3 <- unify (apply s2 t1) (TArrow t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
infer env (Let e1 e2) = do
  (s1, t1) <- infer env e1
  let env' = applyEnv s1 env
      sigma = generalize env' t1
  -- In the let-bound body we “push” the new binding.
  let env'' = Map.insert 0 sigma (shiftEnv 1 env')
  (s2, t2) <- infer env'' e2
  return (s2 `composeSubst` s1, t2)
infer env (Bop _ e1 e2) = do
  (s1, t1) <- infer env e1
  s2 <- unify t1 TInt
  (s3, t2) <- infer (applyEnv s2 env) e2
  s4 <- unify t2 TInt
  return (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, TInt)
infer env (Op _ e) =
  infer env e
infer env (IfZ cond e1 e2) = do
  (s1, tCond) <- infer env cond
  s2 <- unify tCond TInt
  (s3, t1) <- infer (applyEnv s2 env) e1
  (s4, t2) <- infer (applyEnv s3 (applyEnv s2 env)) e2
  s5 <- unify t1 t2
  return (s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, t1)
infer env (Tup e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applyEnv s1 env) e2
  return (s2 `composeSubst` s1, TTuple t1 t2)
infer env (FixP e) = do
  (s1, t) <- infer env e
  tv <- newTyVar
  -- expect the type of e to be an arrow from tv to tv.
  s2 <- unify t (TArrow tv tv)
  return (s2 `composeSubst` s1, apply s2 tv)

-- instantiate a type scheme by replacing quantified type variables with fresh ones
instantiate :: Scheme -> Infer Type
instantiate (Forall vars t) = do
  nvars <- mapM (const newTyVar) vars
  let s = Map.fromList (zip vars nvars)
  return $ apply s t

-- when using de Bruijn indices we need to shift variable indices
shiftEnv :: Int -> TEnv -> TEnv
shiftEnv d = Map.mapKeysMonotonic (+ d)

runInfer :: Infer (Subst, Type) -> Either String Type
runInfer m = case evalState (runExceptT m) 0 of
  Left err -> Left err
  Right (s, t) -> Right (apply s t)
