{
module Parser (main, parseMd) where

import Language
import Lexer (alexScanTokens)
}

%name parseMd
%tokentype { Token }
%error { parseError }

%token
    heading     { THeading $$ }
    "**"        { TBoldDelimiter }
    "*"         { TItalicDelimiter }
    text        { TText $$ }
    "\n"        { TNewLine }
    "- "        { TUnorderedItemStart }
    "n. "       { TOrderedItemStart }
    templ       { TTemplate $$ }
    templend    { TTemplateEnd }
    "["         { TLBracket }
    "]"         { TRBracket }
    "("         { TLHyperlink }
    ")"         { TRHyperlink }
    eof         { TEof }

%%

-- %nonassoc 

RootExpr : heading Expr { Heading $1 $2 }
         | Expr { Body $1 }
         | RootExpr RootExpr { case ($1, $2) of -- Rules for concatenating root expressions
                                (RootSeq res1, RootSeq res2) -> RootSeq (res1 ++ res2)
                                (re1, RootSeq res2) -> RootSeq (re1 : res2)
                                (RootSeq res1, re2) -> RootSeq (res1 ++ [re2])
                                _ -> RootSeq [$1, $2]
                                } 
         | "\n" { NewLine }

Expr : "**" Expr "**" { Bold $2 }
     | "*" Expr "*" { Italic $2 }
     | "[" Expr "]" "(" text ")" { Hyperlink $2 $5 }
     | text { Text $1 }
     | Expr Expr { case ($1, $2) of -- Rules for concatenating expressions
                    (Seq es1, Seq es2) -> Seq (es1 ++ es2)
                    (Seq es1, e2) -> Seq (es1 ++ [e2])
                    (e1, Seq es2) -> Seq (e1 : es2)
                    (Text s1, Text s2) -> Text (s1 ++ s2)
                    _ -> Seq [$1, $2]
                    }
{

parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts

data RootExpr =
    Heading Int Expr |
    Body Expr |
    NewLine |
    RootSeq [RootExpr]
    deriving Show

data Expr =
    Seq [Expr] |
    Text String |
    Bold Expr |
    Italic Expr |
    Hyperlink Expr String |
    OrderedList [Expr] |
    UnorderedList [Expr] |
    Template String |
    Image String String
    deriving Show


main = do
  s <- getContents
  print s
  print $ alexScanTokens s
  print $ parseMd $ (alexScanTokens s)
}