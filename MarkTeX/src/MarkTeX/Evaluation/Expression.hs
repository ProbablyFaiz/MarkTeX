module MarkTeX.Evaluation.Expression where

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
  deriving (Show, Eq)
