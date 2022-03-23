module TemplateLang.Command where

import TemplateLang.Values
import TemplateLang.Boolean

import Data.Data (Typeable)
import qualified Prelude as P
import Prelude hiding (not)

data MetaCommand = If Bool | IfVar String | Insert TValue | InsertVar String | SetVar String TValue | For String TValue | While Bool
  deriving (Read, Show, Typeable)

tIf :: (ToBool a) => a -> MetaCommand
tIf = If . toBool

tIfNot :: (ToBool a) => a -> MetaCommand
tIfNot = If . not
