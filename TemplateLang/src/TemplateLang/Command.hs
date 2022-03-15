module TemplateLang.Command where

import TemplateLang.Values

import Data.Data (Typeable)

data MetaCommand = IfE Bool | If TValue | IfVar String | InsertVar String
  deriving (Read, Show, Typeable)
