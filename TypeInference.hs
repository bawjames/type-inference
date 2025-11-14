module TypeInference (inferAll, infer, runInferAll, runInfer) where

import Control.Monad (foldM)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first, second)
import Data.List (intercalate)
import Data.Map.Lazy qualified as Map
import Data.Set qualified as Set
import ParseTree
import Parser (entry)
import Safe (headMay, tailSafe)
import Text.Parsec.String (parseFromFile)

data Scheme = ForAll (Set.Set Int) Type

instance Show Scheme where
  show (ForAll s t) | Set.null s = show t
  show (ForAll s t) = "∀(" ++ commaSep s ++ ") " ++ show t
    where
      commaSep =
        intercalate ", "
          . map (("t" ++) . show)
          . Set.toList

data Type
  = Function Type Type
  | Concrete Primitive
  | Var Int
  deriving (Eq, Ord)

instance Show Type where
  show (Function a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (Concrete prim) = show prim
  show (Var int) = "t" ++ show int

data Primitive
  = Integer
  | Bool
  deriving (Show, Eq, Ord)

type Environment = Map.Map Ident Scheme

data Constraint = Constraint Type Type
  deriving (Eq, Ord)

type Constraints = Set.Set Constraint

data Substitution = Substitute Type Type
  deriving (Eq, Ord)

type Solution = Set.Set Substitution

instance Show Constraint where
  show (Constraint a b) = show a ++ " = " ++ show b

type IdGen = State Int

freshVar :: IdGen Type
freshVar = gets Var <* modify (+ 1)

inferAll :: Environment -> [Assign] -> MaybeT IdGen Environment
inferAll = foldM generalise

generalise ::
  Environment ->
  Assign ->
  MaybeT IdGen Environment
generalise env (Assign ident expr) = do
  (t, cs) <- infer env expr
  let t' = solve t $ unify cs
  let env' = Map.map (schemeSubst t t') env
  return $ Map.insert ident (ForAll (findVars t') t') env'
  where
    findVars (Function a b) = findVars a <> findVars b
    findVars (Var n) = Set.singleton n
    findVars _ = Set.empty

solve :: Type -> Solution -> Type
solve = Set.foldr $ \(Substitute a b) t -> subst a b t

appearsIn :: Type -> Type -> Bool
appearsIn v@(Var _) t | v == t = True
appearsIn v@(Var _) (Function a b) = v `appearsIn` a || v `appearsIn` b
appearsIn _ _ = False

unify :: Constraints -> Solution
unify cs | Set.null cs = Set.empty
unify cs =
  case Set.elemAt 0 cs of
    Constraint a b | a == b -> unify $ Set.deleteAt 0 cs
    Constraint v@(Var _) t | not $ v `appearsIn` t -> substCs v t
    Constraint t v@(Var _) | not $ v `appearsIn` t -> substCs v t
    Constraint (Function a b) (Function a' b') ->
      unify $
        Set.insert (Constraint a a') $
          Set.insert (Constraint b b') $
            Set.deleteAt 0 cs
  where
    substCs v t =
      Set.insert (Substitute v t) $
        unify $
          flip Set.map cs $ \(Constraint a b) ->
            Constraint (subst v t a) (subst v t b)

instantiate :: Scheme -> IdGen Type
instantiate (ForAll xs t) = do
  foldM substVar t (Set.map Var xs)
  where
    substVar a b = subst a <$> freshVar <*> pure b

schemeSubst :: Type -> Type -> Scheme -> Scheme
schemeSubst v@(Var n) u@(Var m) (ForAll s t) | Set.member n s =
  ForAll (Set.insert m $ Set.delete n s) $ subst v u t
schemeSubst v@(Var n) u (ForAll s t) | Set.member n s =
  ForAll (Set.delete n s) $ subst v u t
schemeSubst v u (ForAll s t) = ForAll s $ subst v u t

subst :: Type -> Type -> Type -> Type
subst a t b | a == b = t
subst n t (Function a b) = Function (subst n t a) (subst n t b)
subst _ _ x = x

infer :: Environment -> Expr -> MaybeT IdGen (Type, Constraints)
infer _ (Literal lit) = return . (,Set.empty) <$> Concrete $
  case lit of
    IntegerLit _ -> Integer
    BoolLit _ -> Bool
infer env (Identifier ident) = do
  let Just t = Map.lookup ident env
  lift $ (,Set.empty) <$> instantiate t
infer env (Lambda bound expr) = do
  var <- lift freshVar
  let env' = Map.insert bound (ForAll Set.empty var) env
  first (Function var) <$> infer env' expr
infer env (Application a b) = do
  (tA, cA) <- infer env a
  (tB, cB) <- infer env b
  var <- lift freshVar
  let cs = Constraint tA (Function tB var) `Set.insert` cA <> cB
  return (var, cs)

runInfer :: Environment -> Expr -> Maybe Type
runInfer env expr = runMaybeT (fst <$> infer env expr) `evalState` 0

runInferAll :: [Assign] -> Maybe Environment
runInferAll expr = runMaybeT (inferAll Map.empty expr) `evalState` 0
