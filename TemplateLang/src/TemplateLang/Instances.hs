module TemplateLang.Instances where

import TemplateLang.Values
import TemplateLang.Number

import Data.Map as M

-- Eq
instance Eq InputValue where
  (==) x1 x2 = case (toNumberMaybe x1, toNumberMaybe x2) of
    (Just n1, Just n2) -> n1 == n2
    _                  -> toString x1 == toString x2

instance Ord InputValue where
  (<=) x1 x2 = case (toNumberMaybe x1, toNumberMaybe x2) of
    (Just n1, Just n2) -> n1 <= n2
    _                  -> case (x1, x2) of
      (IList l1, IList l2)     -> length l1 <= length l2
      (IMap m1, IMap m2)       -> M.size m1 <= M.size m2
      (IString s1, IString s2) -> s1 <= s2
      _                        -> if sameCons x1 x2
        then show x1 <= show x2
        else error $ "Cannot compare " ++ show x1 ++ "to " ++ show x2

instance Num InputValue where
  (+) x1 x2 = INumber $ toNumber x1 + toNumber x2
  (*) x1 x2 = INumber $ toNumber x1 * toNumber x2
  (abs) x = INumber $ abs (toNumber x)
  signum x = INumber $ signum (toNumber x)
  fromInteger x = INumber (fromInteger x)
  negate x = INumber $ negate (toNumber x)

instance Fractional InputValue where
  fromRational x = INumber $ fromRational x
  (/) x1 x2 =  INumber $ toNumber x1 / toNumber x2

instance Real InputValue where
  toRational = toRational . toNumber

