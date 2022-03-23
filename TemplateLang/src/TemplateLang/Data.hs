module TemplateLang.Data where

import TemplateLang.Values

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

toMapMaybe :: TValue -> Maybe TData
toMapMaybe (TData dat) = Just dat
toMapMaybe TNull       = Just M.empty
toMapMaybe x           = Nothing

toMap :: TValue -> TData
toMap x = case toMapMaybe x of
    Just v  -> v
    Nothing -> error ("Not a map: " ++ show x)

-- Allows dot-notation to fetch nested items
lookupTData :: String -> TData -> TValue
lookupTData str = lookupTData' (splitOn "." str) where
    lookupTData' :: [String] -> TData -> TValue
    lookupTData' [] _      = TNull
    lookupTData' [x] tdata = fromMaybe TNull (M.lookup x tdata)
    lookupTData' (x : xs) tdata = case M.lookup x tdata of
        Nothing   -> TNull
        Just tval -> lookupTData' xs (toMap tval)

lookupValue :: String -> TValue -> TValue
lookupValue str x = lookupTData str (toMap x)
