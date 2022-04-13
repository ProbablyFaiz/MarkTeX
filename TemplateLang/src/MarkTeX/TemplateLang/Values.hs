{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module MarkTeX.TemplateLang.Values where

import qualified Data.Map as M

import Data.Data (Typeable)
import GHC.Exts

type TData = M.Map String TValue
data TValue = TString String | TNumber Float | TBool Bool | TList [TValue] | TData TData | TNull
  deriving (Read, Show, Typeable)


-- ToTValue instances
class ToTValue a where
  toTValue :: a -> TValue

instance ToTValue TValue where
  toTValue = id

instance ToTValue Bool where
  toTValue = TBool

instance ToTValue Int where
  toTValue = TNumber . fromIntegral

instance ToTValue Integer where
  toTValue = TNumber . fromIntegral

instance ToTValue Float where
  toTValue = TNumber

instance {-# OVERLAPPABLE #-} (ToTValue a) => ToTValue [a] where
  toTValue xs = TList $ map toTValue $ toList xs

instance {-# OVERLAPPABLE #-} ToTValue String where
  toTValue = TString

-- Prelude instances
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

instance IsList TValue where
    type Item TValue = TValue
    fromList = TList
    fromListN _ = TList
    toList (TList xs)    = xs
    toList (TString str) = map (TString . show) str
    toList x             = error ("Not a list: " ++ show x)

-- Utility functions
sameCons :: TValue -> TValue -> Bool
sameCons (TString _) (TString _) = True
sameCons (TNumber _) (TNumber _) = True
sameCons (TBool _)   (TBool _)   = True
sameCons (TList _)   (TList _)   = True
sameCons (TData _)   (TData _)   = True
sameCons TNull       TNull       = True
sameCons _       _               = False

toString :: TValue -> String
toString (TString str)   = str
toString (TNumber num)   = if isInt num then show $ toInt num else show num
toString (TBool bool)    = show bool
toString (TList vallist) = show vallist
toString (TData dat)     = show dat
toString TNull           = ""

toNumber :: TValue -> Float
toNumber val = case toNumberMaybe val of
  (Just x) -> x
  Nothing  -> error ("NaN: " ++ show val)

toNumberMaybe :: TValue -> Maybe Float
toNumberMaybe (TNumber x)   = Just x
toNumberMaybe (TBool False) = Just 0
toNumberMaybe (TBool True)  = Just 1
toNumberMaybe _             = Nothing

-- Check with 7 decimals of precision
isInt :: RealFrac b => b -> Bool
isInt x = round (10^7*(x-fromIntegral (round x)))==0

toInt :: RealFrac b => b -> Int 
toInt = round
