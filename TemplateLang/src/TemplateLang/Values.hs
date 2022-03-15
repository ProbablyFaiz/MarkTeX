module TemplateLang.Values where

import qualified Data.Map as M

import Data.Data (Typeable)
import Data.Functor.Const (Const)
import Data.Maybe (fromMaybe)

type TData = M.Map String TValue
data TValue = TString String | TNumber Float | TBool Bool | TList [TValue] | TData TData | TNull
  deriving (Read, Show, Typeable)

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
