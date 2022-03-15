module TemplateLang.Instances where

import TemplateLang.Values
import TemplateLang.Number

import Data.Map as M

-- Eq
instance Eq TValue where
  (==) x1 x2 = case (toNumberMaybe x1, toNumberMaybe x2) of
    (Just n1, Just n2) -> n1 == n2
    _                  -> toString x1 == toString x2

instance Ord TValue where
  (<=) x1 x2 = case (toNumberMaybe x1, toNumberMaybe x2) of
    (Just n1, Just n2) -> n1 <= n2
    _                  -> case (x1, x2) of
      (TList l1, TList l2)     -> length l1 <= length l2
      (TData m1, TData m2)     -> M.size m1 <= M.size m2
      (TString s1, TString s2) -> s1 <= s2
      _                        -> if sameCons x1 x2
        then show x1 <= show x2
        else error $ "Cannot compare " ++ show x1 ++ "to " ++ show x2

instance Num TValue where
  (+) x1 x2 = TNumber $ toNumber x1 + toNumber x2
  (*) x1 x2 = TNumber $ toNumber x1 * toNumber x2
  (abs) x = TNumber $ abs (toNumber x)
  signum x = TNumber $ signum (toNumber x)
  fromInteger x = TNumber (fromInteger x)
  negate x = TNumber $ negate (toNumber x)

instance Fractional TValue where
  fromRational x = TNumber $ fromRational x
  (/) x1 x2 =  TNumber $ toNumber x1 / toNumber x2

instance Real TValue where
  toRational = toRational . toNumber

