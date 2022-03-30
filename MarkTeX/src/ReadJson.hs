import Data.Aeson (eitherDecode, encode, decode, Object(..), Value(..))

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as SC

import TemplateLang

data ReadJsonError = DecodeJson String
                   | ConvertJson String
    deriving (Show)

readJson :: String -> IO (Either ReadJsonError TData)
readJson fileName = do
    bytes <- BS.readFile fileName
    return (bytesToTData bytes)

-- | `bytesToTData` converts a bytestring into a TData map.
-- Can possibly return an error if either decoding the json fails or converting to TData fails.
bytesToTData :: BS.ByteString -> Either ReadJsonError TData
bytesToTData bytes = 
    case eitherDecode bytes of
        Left err -> Left $ DecodeJson err
        Right jsonObject -> Right $ process jsonObject

-- slightly modified answer of the following:
-- https://stackoverflow.com/questions/53579583/parsing-json-to-map-string-string-with-aeson
process :: Value -> TData
process (Object m) = (M.fromList . map f) (HM.toList m)
     where f (x, y) = (T.unpack x, getVal y)
process _ = M.empty

getVal :: Value -> TValue
getVal (Object m) = TData $ process (Object m)
getVal (Array v)  = TList $ map getVal (V.toList v)
getVal (String t) = TString $ T.unpack t
getVal (Number n) = TNumber $ fromInteger coef * 10 ^^ base
    where
        coef = SC.coefficient n
        base = SC.base10Exponent n
getVal (Bool b)   = TBool b       
getVal Null       = TNull
