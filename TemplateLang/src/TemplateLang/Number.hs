module TemplateLang.Number where

import TemplateLang.Values

import qualified Data.Map as M

toNumber :: TValue -> Float
toNumber val = case toNumberMaybe val of
  (Just x) -> x
  Nothing  -> error ("NaN: " ++ show val)

toNumberMaybe :: TValue -> Maybe Float
toNumberMaybe (TNumber x)   = Just x
toNumberMaybe (TBool False) = Just 0
toNumberMaybe (TBool True)  = Just 1
toNumberMaybe val = Nothing
