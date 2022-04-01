{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MarkTeX.TemplateLang.Values where

import qualified Data.Map as M

import Data.Data (Typeable)
import Data.Functor.Const (Const)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import GHC.Exts

type TData = M.Map String TValue
data TValue = TString String | TNumber Float | TBool Bool | TList [TValue] | TData TData | TNull
  deriving (Read, Show, Typeable)

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
toString (TNumber num)   = show num
toString (TBool bool)    = show bool
toString (TList vallist) = show vallist
toString (TData dat)     = show dat
toString TNull           = ""
