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
  | Template String
  | RootSeq [RootExpr]
  deriving (Show, Eq)

data Expr
  = Seq [Expr]
  | Text String
  | Bold Expr
  | Italic Expr
  | Hyperlink Expr String
  | Image Expr String
  deriving (Show, Eq)
