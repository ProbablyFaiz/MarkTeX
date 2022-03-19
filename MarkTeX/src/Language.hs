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
    TNewLine |
    TEof
    deriving (Eq,Show)