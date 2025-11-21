module AlgorithmW (runVarSupply, infer, inferLet, inferAll) where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set ((\\))
import Data.Set qualified as Set
import ParseTree

--
-- Basic data type definitions
--

data Scheme = ForAll (Set.Set Int) Type

data Type
  = Function Type Type
  | Concrete Primitive
  | Var Int
  deriving (Eq, Ord)

data Primitive
  = Integer
  | Bool
  deriving (Show, Eq, Ord)

type Environment = Map.Map Ident Scheme

--
-- `Subst` definition
--

type Subst = Map.Map Int Type

compose :: Subst -> Subst -> Subst
compose sA sB = Map.map (apply sA) sB <> sA

--
-- `Show` instances
--

instance Show Scheme where
  show (ForAll s t) | Set.null s = show t
  show (ForAll s t) = "∀(" ++ commaSep s ++ ") " ++ show t
    where
      commaSep =
        intercalate ", "
          . map (("t" ++) . show)
          . Set.toList

instance Show Type where
  show (Function a b) = showLeft a ++ " -> " ++ show b
    where
      showLeft f@(Function _ _) = "(" ++ show f ++ ")"
      showLeft t = show t
  show (Concrete prim) = show prim
  show (Var int) = "t" ++ show int

--
-- `Types` instances
--

class Types a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Int

instance (Types a) => Types [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Types Environment where
  apply s = Map.map $ apply s
  ftv = ftv . Map.elems

instance Types Scheme where
  apply s (ForAll vars t) =
    ForAll vars $
      apply (foldr Map.delete s vars) t
  ftv (ForAll vars t) = ftv t \\ vars

instance Types Type where
  apply s v@(Var n) = fromMaybe v $ Map.lookup n s
  apply s (Function tA tB) = Function (apply s tA) (apply s tB)
  apply s t = t

  ftv (Function tA tB) = ftv tA <> ftv tB
  ftv (Var n) = Set.singleton n
  ftv _ = Set.empty

generalise :: Environment -> Type -> Scheme
generalise env t = ForAll vars t
  where
    vars = ftv t \\ ftv env

--
-- Monadic functions
--

type VarSupply = ExceptT String (State Int)

freshVar :: VarSupply Type
freshVar = gets Var <* modify (+ 1)

instantiate :: Scheme -> VarSupply Type
instantiate (ForAll vars t) = do
  let vars' = Set.toList vars
  let zipVars = mapM $ \var -> (var,) <$> freshVar

  s <- Map.fromList <$> zipVars vars'
  return $ apply s t

mgu :: Type -> Type -> VarSupply Subst
mgu (Function l r) (Function l' r') = do
  sA <- mgu l l'
  sB <- mgu (apply sA r) (apply sA r')
  return $ sA `compose` sB
mgu (Var u) t = varBind u t
mgu t (Var u) = varBind u t
mgu (Concrete a) (Concrete b) | a == b = return Map.empty
mgu tA tB =
  throwError $
    "Types do not unify, " ++ show tA ++ " vs. " ++ show tB

varBind :: Int -> Type -> VarSupply Subst
varBind u t
  | t == Var u = return Map.empty
  | u `Set.member` ftv t =
      throwError $
        "Infinite type recursion, " ++ show u ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton u t

infer :: Environment -> Expr -> VarSupply (Subst, Type)
infer env (Identifier ident) =
  case Map.lookup ident env of
    Just sig -> (Map.empty,) <$> instantiate sig
    Nothing ->
      throwError $
        "Couldn't find `" ++ ident ++ "`, undefined or out of scope"
infer env (Literal lit) = return . (Map.empty,) <$> Concrete $
  case lit of
    IntegerLit _ -> Integer
    BoolLit _ -> Bool
infer env (Lambda bound body) = do
  newVar <- freshVar
  let env' = Map.insert bound (ForAll Set.empty newVar) env
  (s, t) <- infer env' body
  return (s, apply s (Function (apply s newVar) t))
infer env (Application eA eB) = do
  newVar <- freshVar
  (sA, tA) <- infer env eA
  (sB, tB) <- infer (apply sA env) eB
  sC <- mgu (apply sB tA) (Function tB newVar)
  return (sC `compose` sB `compose` sA, apply sC newVar)

inferLet :: Environment -> Assign -> VarSupply Environment
inferLet env (Let ident expr) = do
  (sA, tA) <- infer env expr
  let t' = generalise (apply sA env) tA
  return $ Map.insert ident t' env

inferAll :: Environment -> [Assign] -> VarSupply Environment
inferAll = foldM inferLet

runVarSupply :: VarSupply a -> Either String a
runVarSupply t = (`evalState` 0) $ runExceptT t
