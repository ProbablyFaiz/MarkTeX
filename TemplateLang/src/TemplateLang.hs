module TemplateLang (
        module TemplateLang.Boolean,
        module TemplateLang.List,
        module TemplateLang.Data,
        module TemplateLang.Number,
        module TemplateLang.Values,
        module TemplateLang.Instances,
        module TemplateLang.Command,
        hidePreludeString
    ) where

import TemplateLang.Boolean
import TemplateLang.List
import TemplateLang.Data
import TemplateLang.Number
import TemplateLang.Values
import TemplateLang.Instances
import TemplateLang.Command
import Data.List

hidePreludeString :: String
hidePreludeString = "(" ++ intercalate ", " hidePreludeFunctions  ++ ")"

hidePreludeFunctions :: [String]
hidePreludeFunctions = ["(||)", "(&&)", "not"]
