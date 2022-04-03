{-# LANGUAGE DeriveDataTypeable #-}
module MarkTeX.TemplateLang.Expression where
import Data.Data (Data)

data Expr
  = Heading Int Expr
  | OrderedList [Expr]
  | UnorderedList [Expr]
  | NewLine
  | Seq [Expr]
  | Text String
  | Bold Expr
  | Italic Expr
  | Hyperlink Expr Expr
  | Image Expr Expr
  deriving (Show, Read, Eq, Data)