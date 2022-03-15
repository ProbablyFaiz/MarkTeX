module TemplateLang.Boolean where

import TemplateLang.Values
import Data.Boolean
import qualified Data.Map as M

toBool :: InputValue -> Bool
toBool (IString str) = str /= ""
toBool (INumber num) = num /= 0
toBool (IBool bool)  = bool
toBool (IList xs)    = not (null xs)
toBool (IMap imap)   = M.size imap > 0
toBool INull         = False

fromBool :: Bool -> InputValue
fromBool True = true
fromBool False = false

instance Boolean InputValue where
  true = IBool True
  false = IBool False
  notB = IBool . not . toBool
  (&&*) x1 x2 = IBool $ toBool x1 && toBool x2
  (||*) x1 x2 = IBool $ toBool x1 || toBool x2