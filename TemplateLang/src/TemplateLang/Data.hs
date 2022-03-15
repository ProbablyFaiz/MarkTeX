module TemplateLang.Data where

import TemplateLang.Values

import qualified Data.Map as M
import Data.Maybe

toMap :: TValue -> TData
toMap (TData dat) = dat
toMap TNull = M.empty
toMap x = error ("Not a map: " ++ show x)

lookupTData :: String -> TData -> TValue
lookupTData str m = fromMaybe TNull (M.lookup str m)

lookupValue :: String -> TValue -> TValue
lookupValue str x = lookupTData str (toMap x)
