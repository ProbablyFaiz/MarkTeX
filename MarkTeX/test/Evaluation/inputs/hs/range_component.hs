module RangeComponent where

import MarkTeX.TemplateLang

rangeComponent :: Int -> Int -> MetaCommand
rangeComponent start end = InsertExpr $ UnorderedList (rangeComponent' start) where
    rangeComponent' :: Int -> [Expr]
    rangeComponent' curr = if curr <= end
        then Text (show curr) : rangeComponent' (curr + 1)
        else []
