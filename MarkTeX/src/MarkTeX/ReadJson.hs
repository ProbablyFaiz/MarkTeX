-- | The 'MarkTeX.ReadJson' module provides functionality to read data from JSON files.
-- The 'readJson' function returns an error if the file cannot be found.
-- The 'readOptionalJson' function returns an empty environment when the specified file can not be found.
module MarkTeX.ReadJson (readJson, readOptionalJson, ReadJsonError(..)) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as SC

import Data.Aeson (eitherDecode, Value(..))
import Data.Functor ((<&>))
import System.Directory (doesFileExist)

import MarkTeX.TemplateLang

-- | The 'ReadJsonError' datatype catches the possible error when the decoding of the JSON fails.
-- It can also return that a file does not exist if the specified file is expected to be found.
data ReadJsonError = DecodeJson String
                   | FileDoesNotExist FilePath
    deriving (Show)

-- | The function 'readOptionalJson' reads data from a json file if the file exists.
-- If the file does not exist an empty environment is returned.
readOptionalJson :: Maybe String -> IO (Either ReadJsonError TData)
readOptionalJson Nothing = return $ Right M.empty
readOptionalJson (Just fileName) = do
    fileExists <- doesFileExist fileName
    if fileExists
        then BS.readFile fileName <&> bytesToTData
        else return $ Left $ FileDoesNotExist fileName

-- | The function 'readOptionalJson' reads data from a json file if the file exists.
-- If the file does not exist it returns a 'FileDoesNotExist' error.
readJson :: String -> IO (Either ReadJsonError TData)
readJson fileName = do
    fileExists <- doesFileExist fileName
    if fileExists
        then BS.readFile fileName <&> bytesToTData
        else return $ Left $ FileDoesNotExist fileName

-- | The `bytesToTData` function converts a bytestring into a TData map.
-- Can possibly return an error if either decoding the json fails or converting to TData fails.
bytesToTData :: BS.ByteString -> Either ReadJsonError TData
bytesToTData bytes = 
    case eitherDecode bytes of
        Left err -> Left $ DecodeJson err
        Right jsonObject -> Right $ process jsonObject

-- | The 'process' function takes a JSON input argument, which has the 'Value' type from usage of the decoding provided by the 'Data.Aeson' library.
-- This input JSON datatype is then converted to our 'TData' format.
-- This specific 'process' function is a slightly modified of the answer found on https://stackoverflow.com/questions/53579583/parsing-json-to-map-string-string-with-aeson.
process :: Value -> TData
process (Object m) = (M.fromList . map f) (HM.toList m)
     where f (x, y) = (T.unpack x, getVal y)
process _ = M.empty

-- | The function 'getVal' converts all possible JSON values of the 'Value' type to the 'TValue' type.
-- This is a helper function to the 'process' function.
getVal :: Value -> TValue
getVal (Object m) = TData $ process (Object m)
getVal (Array v)  = TList $ map getVal (V.toList v)
getVal (String t) = TString $ T.unpack t
getVal (Number n) = TNumber $ fromInteger coef * 10 ^^ base where
    coef = SC.coefficient n
    base = SC.base10Exponent n
getVal (Bool b)   = TBool b       
getVal Null       = TNull
