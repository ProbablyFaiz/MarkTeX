module TemplateLang.Boolean where

import TemplateLang.Values
import Data.Boolean
import qualified Data.Map as M

toBool :: TValue -> Bool
toBool (TString str) = str /= ""
toBool (TNumber num) = num /= 0
toBool (TBool bool)  = bool
toBool (TList xs)    = not (null xs)
toBool (TData dat)   = M.size dat > 0
toBool TNull         = False

fromBool :: Bool -> TValue
fromBool True = true
fromBool False = false

instance Boolean TValue where
  true = TBool True
  false = TBool False
  notB = TBool . not . toBool
  (&&*) x1 x2 = TBool $ toBool x1 && toBool x2
  (||*) x1 x2 = TBool $ toBool x1 || toBool x2