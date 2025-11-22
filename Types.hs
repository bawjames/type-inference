module Types where

import Data.List (intercalate)
import Data.Map.Lazy qualified as Map
import Data.Tuple (snd)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as Pr

-- This is just a trie which allows a trie as keys
-- A Type of nested map singletons is equivalent to a functional type signature in functional languages
-- An empty Type is not valid
-- A non-singleton, non-empty Type expresses a superposition of types, and the type can eventually be found by repeated application
data Type
  = TyFun (Map.Map Type Type)
  | TyVar Int
  | TyCon Primitive
  deriving (Eq, Ord)

-- Type is not a Monoid because there is no empty Type, therefore no mempty.
instance Semigroup Type where
  TyFun a <> TyFun b = TyFun $ Map.unionWith (<>) a b

instance Show Type where
  show = Pr.render . toDoc
    where
      toDoc (TyVar n) = Pr.text $ "t" ++ show n
      toDoc (TyCon p) = Pr.text $ show p
      toDoc (TyFun m) = Pr.vcat $
        flip map (Map.toList m) $
          \(k, v) -> parenLeft k <+> Pr.text "->" <+> toDoc v
      parenLeft ty@(TyFun _) = Pr.parens $ toDoc ty
      parenLeft ty = toDoc ty

data Primitive
  = TyInt
  | TyDouble
  | TyFloat
  | TyBool
  deriving (Eq, Ord, Show)

fromList :: [Type] -> Type
fromList = foldr1 $ fmap TyFun . Map.singleton

apply :: Type -> Type -> Maybe Type
apply (TyFun m) a = Map.lookup a m
apply _ _ = Nothing

bool = TyCon TyBool
double = TyCon TyDouble
float = TyCon TyFloat
int = TyCon TyInt

go :: IO ()
go =
  print $ apply x (a <> b)
  where
    x = a <> b <> c
    a = fromList [int, int, int]
    b = fromList [int, float, float]
    c = fromList [a <> b, int, float]
