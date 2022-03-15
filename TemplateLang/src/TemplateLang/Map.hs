module TemplateLang.Map where

import TemplateLang.Values

import qualified Data.Map as M
import Data.Maybe

toMap :: InputValue -> IMap
toMap (IMap imap) = imap
toMap INull = M.empty
toMap x = error ("Not a map: " ++ show x)

lookupIMap :: String -> IMap -> InputValue
lookupIMap str m = fromMaybe INull (M.lookup str m)

lookupValue :: String -> InputValue -> InputValue
lookupValue str x = lookupIMap str (toMap x)
