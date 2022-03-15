module TemplateLang.Number where

import TemplateLang.Values

import qualified Data.Map as M

toNumber :: InputValue -> Float
toNumber val = case toNumberMaybe val of
  (Just x) -> x
  Nothing  -> error ("NaN: " ++ show val)

toNumberMaybe :: InputValue -> Maybe Float
toNumberMaybe (INumber x)   = Just x
toNumberMaybe (IBool False) = Just 0
toNumberMaybe (IBool True)  = Just 1
toNumberMaybe val = Nothing
