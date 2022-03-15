module TemplateLang.Command where

import TemplateLang.Values

import Data.Data (Typeable)

data MetaCommand = IfE Bool | If InputValue | IfVar String | InsertVar String
  deriving (Read, Show, Typeable)
