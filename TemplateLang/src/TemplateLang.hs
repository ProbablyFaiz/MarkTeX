module TemplateLang (
        module TemplateLang.Boolean,
        module TemplateLang.List,
        module TemplateLang.Data,
        module TemplateLang.Number,
        module TemplateLang.Values,
        module TemplateLang.Instances,
        module TemplateLang.Command,
        module TemplateLang.ParseLookup,
        hidePreludeString
    ) where

import TemplateLang.Boolean
import TemplateLang.List
import TemplateLang.Data
import TemplateLang.Number
import TemplateLang.Values
import TemplateLang.Instances
import TemplateLang.Command
import TemplateLang.ParseLookup
import Data.List

hidePreludeString :: String
hidePreludeString = "(" Prelude.++ intercalate ", " hidePreludeFunctions Prelude.++ ")"

hidePreludeFunctions :: [String]
hidePreludeFunctions = ["(||)", "(&&)", "not", "(++)"]
