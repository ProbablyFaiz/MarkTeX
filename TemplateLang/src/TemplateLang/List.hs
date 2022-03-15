module TemplateLang.List where

import TemplateLang.Values

toList :: TValue -> [TValue]
toList (TList xs)    = xs
toList (TString str) = map (TString . show) str
toList x             = error ("Not a list: " ++ show x)

foldr :: (TValue -> TValue -> TValue) -> TValue -> TValue -> TValue
foldr f x xs = Prelude.foldr f x (toList xs)