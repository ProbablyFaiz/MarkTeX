module MarkTeX.Parsing.Expression where

data Token
  = THeading Int
  | TImageStart
  | TLBracket
  | TRBracket
  | TLHyperlink
  | TRHyperlink
  | TText String
  | TBoldDelimiter
  | TItalicDelimiter
  | TUnorderedItemStart
  | TOrderedItemStart
  | TCommand String
  | TCommandBlockStart String
  | TCommandBlockEnd
  | TNewLine
  deriving (Show, Eq)

data RootExpr
  = Heading Int Expr
  | Body Expr
  | OrderedList [Expr]
  | UnorderedList [Expr]
  | NewLine
  | CommandBlockCode String RootExpr
  | RootSeq [RootExpr]
  deriving (Show, Eq)

data Expr
  = Seq [Expr]
  | Text String
  | Bold Expr
  | Italic Expr
  | Hyperlink Expr Expr
  | Image Expr Expr
  | CommandCode String
  deriving (Show, Eq)


