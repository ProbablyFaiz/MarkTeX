{-# LANGUAGE FlexibleInstances #-}
module Templating.Commands where

import qualified Data.Map as M

import Data.Boolean
import Data.Data (Typeable)
import Data.Functor.Const (Const)
import Data.Maybe (fromMaybe)

type IMap = M.Map String InputValue
data InputValue = IString String | INumber Float | IBool Bool | IList [InputValue] | IMap IMap | INull
  deriving (Read, Show, Typeable)

data MetaCommand = IfE Bool | If InputValue | IfVar String | InsertVar String
  deriving (Read, Show, Typeable)

-- Utility functions
sameCons :: InputValue -> InputValue -> Bool
sameCons (IString _) (IString _) = True
sameCons (INumber _) (INumber _) = True
sameCons (IBool _)   (IBool _)   = True
sameCons (IList _)   (IList _)   = True
sameCons (IMap _)    (IMap _)    = True
sameCons INull   INull           = True
sameCons _       _               = False

toString :: InputValue -> String
toString (IString str)   = str
toString (INumber num)   = show num
toString (IBool bool)    = show bool
toString (IList vallist) = show vallist
toString (IMap imap)     = show imap
toString INull           = ""

-- Eq
instance Eq InputValue where
  (==) x1 x2 = case (toNumberMaybe x1, toNumberMaybe x2) of
    (Just n1, Just n2) -> n1 == n2
    _                  -> toString x1 == toString x2

-- Ord
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

-- Number computations on input values
toNumber :: InputValue -> Float
toNumber val = case toNumberMaybe val of
  (Just x) -> x
  Nothing  -> error ("NaN: " ++ show val)

toNumberMaybe :: InputValue -> Maybe Float
toNumberMaybe (INumber x)   = Just x
toNumberMaybe (IBool False) = Just 0
toNumberMaybe (IBool True)  = Just 1
toNumberMaybe val = Nothing

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

-- Boolean computations on input values
-- All values can be converted to booleans for ease of use
toBool :: InputValue -> Bool
toBool (IString str) = str /= ""
toBool (INumber num) = num /= 0
toBool (IBool bool)  = bool
toBool (IList xs)    = not (null xs)
toBool (IMap imap)   = M.size imap > 0
toBool INull         = False

fromBool :: Bool -> InputValue
fromBool True = true
fromBool False = false

instance Boolean InputValue where
  true = IBool True
  false = IBool False
  notB = IBool . not . toBool
  (&&*) x1 x2 = IBool $ toBool x1 && toBool x2
  (||*) x1 x2 = IBool $ toBool x1 || toBool x2

-- List computations
toList :: InputValue -> [InputValue]
toList (IList xs)    = xs
toList (IString str) = map (IString . show) str
toList x             = error ("Not a list: " ++ show x)

foldr :: (InputValue -> InputValue -> InputValue) -> InputValue -> InputValue -> InputValue
foldr f x xs = Prelude.foldr f x (toList xs)

-- Map computations
toMap :: InputValue -> IMap
toMap (IMap imap) = imap
toMap INull = M.empty
toMap x = error ("Not a map: " ++ show x)

lookupIMap :: String -> IMap -> InputValue
lookupIMap str m = fromMaybe INull (M.lookup str m)

lookupValue :: String -> InputValue -> InputValue
lookupValue str x = lookupIMap str (toMap x)
