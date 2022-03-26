module Language where

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
  | TTemplate String
  | TTemplateBlockStart String
  | TTemplateBlockEnd
  | TNewLine
  deriving (Show, Eq)

data RootExpr
  = Heading Int Expr
  | Body Expr
  | OrderedList [Expr]
  | UnorderedList [Expr]
  | NewLine
  | TemplateBlock String RootExpr
  | RootSeq [RootExpr]
  deriving (Show, Eq)

data Expr
  = Seq [Expr]
  | Text String
  | Bold Expr
  | Italic Expr
  | Hyperlink Expr Expr
  | Image Expr Expr
  | Template String
  deriving (Show, Eq)

