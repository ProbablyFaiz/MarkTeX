module MarkTeX.TemplateLang (
        module MarkTeX.TemplateLang.Boolean,
        module MarkTeX.TemplateLang.List,
        module MarkTeX.TemplateLang.Data,
        module MarkTeX.TemplateLang.Number,
        module MarkTeX.TemplateLang.Values,
        module MarkTeX.TemplateLang.Instances,
        module MarkTeX.TemplateLang.Command,
        module MarkTeX.TemplateLang.ParseLookup,
        hidePreludeString
    ) where

import MarkTeX.TemplateLang.Boolean
import MarkTeX.TemplateLang.List
import MarkTeX.TemplateLang.Data
import MarkTeX.TemplateLang.Number
import MarkTeX.TemplateLang.Values
import MarkTeX.TemplateLang.Instances
import MarkTeX.TemplateLang.Command
import MarkTeX.TemplateLang.ParseLookup
import Data.List

hidePreludeString :: String
hidePreludeString = "(" Prelude.++ intercalate ", " hidePreludeFunctions Prelude.++ ")"

hidePreludeFunctions :: [String]
hidePreludeFunctions = ["(||)", "(&&)", "not", "(++)"]
