module Types where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map
import Text.Pretty.Simple (pPrint)

-- This is just a trie which allows a trie as keys
-- A Type of nested map singletons is equivalent to a functional type signature in functional languages
-- An empty Type is not valid
-- A non-singleton, non-empty Type expresses a superposition of types, and the type can eventually be found by repeated application
data Type
  = TyFun (Map.Map Type Type)
  | TyVar Int
  | TyCon Primitive
  deriving (Eq, Ord, Show)

instance Semigroup Type where
  TyFun mA <> TyFun mB = TyFun $ Map.unionWith (<>) mA mB

-- instance Show Type where
--   show (TyCon c) = show c
--   show (TyVar n) = "t" ++ show n
--   show (TyFun m) = unlines $ draw $ TyFun m

-- draw :: Type -> [String]
-- draw (TyFun m) = map (\(k, v) -> show k ++ intercalate "" (draw v)) $ Map.toList m
-- draw x = [show x]

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

go :: IO ()
-- go = pPrint $ apply float $ mergeTrees a b
go = do
  let x = a <> b <> c
  let y = apply x float
  pPrint x
  pPrint y
  where
    a = fromList [float, float, float]
    b = fromList [float, double, double]
    c = fromList [double, double, double]
    int = TyCon TyInt
    double = TyCon TyDouble
    float = TyCon TyFloat
    bool = TyCon TyBool
