module Language where

data Token =
    THeading Int |
    TLBracket |
    TRBracket |
    TLHyperlink |
    TRHyperlink |
    TText String |
    TBoldDelimiter |
    TItalicDelimiter |
    TUnorderedItemStart |
    TOrderedItemStart |
    TTemplate String |
    TTemplateEnd |
    TNewLine
    deriving (Eq,Show)

data RootExpr =
    Heading Int Expr |
    Body Expr |
    OrderedList [Expr] |
    UnorderedList [Expr] |
    NewLine |
    Template String |
    RootSeq [RootExpr]
    deriving Show

data Expr =
    Seq [Expr] |
    Text String |
    Bold Expr |
    Italic Expr |
    Hyperlink Expr Expr |
    Image String String
    deriving Show