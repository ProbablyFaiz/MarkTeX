{-# LANGUAGE DeriveDataTypeable #-}

-- | The 'MarkTeX.Parsing.Expression' module contains the datatypes for the lexer tokens 'Token' and the parser output expressions 'Expr' and 'RootExpr'.
module MarkTeX.Parsing.Expression where
import Data.Data (Data)

-- | The datatype 'Token' contains all tokens that are parsed by the MarkTeX lexer.
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
  | TCodeSnippet String
  | TNewLine
  deriving (Show, Eq)

-- | The datatype 'RootExpr' contains all the possible root expressions of the MarkTeX language.
-- These root expressions can contain simple expressions which are listed under the 'Expr' datatype.
data RootExpr
  = Heading Int Expr
  | Body Expr
  | OrderedList [Expr]
  | UnorderedList [Expr]
  | NewLine
  | CommandBlockCode String RootExpr
  | CodeSnippet String
  | RootSeq [RootExpr]
  deriving (Show, Eq, Data)

-- | The datatype 'Expr' contains all the possible simple expressions of the MarkTeX language.
data Expr
  = Seq [Expr]
  | Text String
  | Bold Expr
  | Italic Expr
  | Hyperlink Expr Expr
  | Image Expr Expr
  | CommandCode String
  deriving (Show, Eq, Data)

type ParseError = String
