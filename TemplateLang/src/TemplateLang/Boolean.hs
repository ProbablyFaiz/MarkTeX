module TemplateLang.Boolean where

import TemplateLang.Values

import qualified Prelude as P
import Prelude hiding (not)
import qualified Data.Map as M

class ToBool a where
  toBool :: a -> Bool

(||) :: (ToBool a, ToBool b) => a -> b -> Bool
(||) b1 b2 = toBool b1 P.|| toBool b2

(&&) :: (ToBool a, ToBool b) => a -> b -> Bool
(&&) b1 b2 = toBool b1 P.&& toBool b2

not :: (ToBool a) => a -> Bool
not = P.not . toBool

instance ToBool Bool where
  toBool = id

instance ToBool TValue where
  toBool (TString str) = str /= ""
  toBool (TNumber num) = num /= 0
  toBool (TBool bool)  = bool
  toBool (TList xs)    = not (null xs)
  toBool (TData dat)   = M.size dat > 0
  toBool TNull         = False
