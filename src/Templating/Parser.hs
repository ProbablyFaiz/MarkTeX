module Templating.Parser where

type Template = [TExpr]
data TExpr = Seq [TExpr] | Command String | Block String TExpr | Text String
