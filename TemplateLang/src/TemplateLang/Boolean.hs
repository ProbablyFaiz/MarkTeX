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

-- instance ToBool Int where
--   toBool = (/=) 0 

-- instance ToBool Integer where
--   toBool = (/=) 0 

-- instance ToBool Float where
--   toBool = (/=) 0 

-- instance ToBool [a] where
--   toBool [] = True
--   toBool _  = False

instance ToBool TValue where
  toBool (TString str) = str /= "" --toBool str
  toBool (TNumber num) = num /= 0  --toBool num
  toBool (TBool bool)  = bool
  toBool (TList xs)    = not (null xs) --toBool xs
  toBool (TData dat)   = M.size dat > 0
  toBool TNull         = False
