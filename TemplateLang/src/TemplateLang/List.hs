module TemplateLang.List where

import TemplateLang.Values

toList :: InputValue -> [InputValue]
toList (IList xs)    = xs
toList (IString str) = map (IString . show) str
toList x             = error ("Not a list: " ++ show x)

foldr :: (InputValue -> InputValue -> InputValue) -> InputValue -> InputValue -> InputValue
foldr f x xs = Prelude.foldr f x (toList xs)