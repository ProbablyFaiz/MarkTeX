module MarkTeX.TemplateLang (
        module MarkTeX.TemplateLang.Expression,
        module MarkTeX.TemplateLang.Boolean,
        module MarkTeX.TemplateLang.List,
        module MarkTeX.TemplateLang.Data,
        module MarkTeX.TemplateLang.Values,
        module MarkTeX.TemplateLang.Command,
        module MarkTeX.TemplateLang.ParseLookup,
        hidePreludeString
    ) where

import MarkTeX.TemplateLang.Expression
import MarkTeX.TemplateLang.Boolean
import MarkTeX.TemplateLang.List
import MarkTeX.TemplateLang.Data
import MarkTeX.TemplateLang.Values
import MarkTeX.TemplateLang.Command
import MarkTeX.TemplateLang.ParseLookup
import Data.List

hidePreludeString :: String
hidePreludeString = "(" Prelude.++ intercalate ", " hidePreludeFunctions Prelude.++ ")"

hidePreludeFunctions :: [String]
hidePreludeFunctions = ["(||)", "(&&)", "not", "(++)"]
