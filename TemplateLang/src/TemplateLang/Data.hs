module TemplateLang.Data where

import TemplateLang.Values

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import TemplateLang.ParseLookup (parseLookup, Lookup(..))

toMapMaybe :: TValue -> Maybe TData
toMapMaybe (TData dat) = Just dat
toMapMaybe TNull       = Just M.empty
toMapMaybe x           = Nothing

toListMaybe :: TValue -> Maybe [TValue]
toListMaybe (TList xs) = Just xs
toListMaybe TNull      = Just []
toListMaybe x          = Nothing

toMap :: TValue -> TData
toMap x = case toMapMaybe x of
    Just v  -> v
    Nothing -> error ("Not a map: " ++ show x)

-- Allows dot-notation to fetch nested items
lookupTData :: String -> TData -> TValue
lookupTData str tData = case parseLookup str of
    Left  () -> TNull
    Right ls -> lookupsInTData ls tData

lookupsInTData :: [Lookup] -> TData -> TValue
lookupsInTData ls tData = lookupsInTValue ls (TData tData)

lookupsInTValue :: [Lookup] -> TValue -> TValue
lookupsInTValue []       _    = TNull
lookupsInTValue [l]      tVal = fromMaybe TNull (lookupInTValue l tVal)
lookupsInTValue (l : ls) tVal =
    case lookupInTValue l tVal of
        Nothing      -> TNull
        Just nextVal -> lookupsInTValue ls nextVal

lookupInTValue :: Lookup -> TValue -> Maybe TValue
lookupInTValue (Name x) tVal =
    case toMapMaybe tVal of
        Nothing      -> Nothing
        (Just tdata) -> M.lookup x tdata
lookupInTValue (Index n) tVal =
    case toListMaybe tVal of
        Nothing   -> Nothing
        (Just vs) ->
            if n >= length vs
                then Nothing
                else Just (vs !! n)

lookupTValue :: String -> TValue -> TValue
lookupTValue str x = lookupTData str (toMap x)
