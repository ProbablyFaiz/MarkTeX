{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module MarkTeX.TemplateLang.List where

import MarkTeX.TemplateLang.Values

import GHC.Exts
import qualified Prelude as P
import Prelude hiding ((++))

class Concat a b c where
    (++) :: a -> b -> c

instance (IsList a) => Concat a a a where
    (++) xs ys = fromList $ toList xs P.++ toList ys

instance (IsList a, ToTValue (Item a)) => Concat TValue a TValue where
    (++) xs ys = fromList (toList xs P.++ map toTValue (toList ys))

instance (IsList a, ToTValue (Item a)) => Concat a TValue TValue where
    (++) xs ys = fromList (map toTValue (toList xs) P.++ toList ys)
