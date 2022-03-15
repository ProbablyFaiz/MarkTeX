{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module TemplateLang.List where

import TemplateLang.Values

import GHC.Exts
import qualified Prelude as P
import Prelude hiding ((++))
import Data.Type.Equality

instance IsList TValue where
    type Item TValue = TValue
    fromList = TList
    fromListN _ = TList
    toList (TList xs)    = xs
    toList (TString str) = map (TString . show) str
    toList x             = error ("Not a list: " ++ show x)

class Concat a b c where
    (++) :: a -> b -> c

instance (IsList a) => Concat a a a where
    (++) xs ys = fromList $ toList xs P.++ toList ys

instance (IsList a, ToTValue (Item a)) => Concat TValue a TValue where
    (++) xs ys = fromList (toList xs P.++ map toTValue (toList ys))

instance (IsList a, ToTValue (Item a)) => Concat a TValue TValue where
    (++) xs ys = fromList (map toTValue (toList xs) P.++ toList ys)
