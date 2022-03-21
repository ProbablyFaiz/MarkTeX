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


%nonassoc BLOCK
%nonassoc COMMAND
%nonassoc DATA
%nonassoc FORMAT
%nonassoc CONCAT

%%

RootExpr : heading Expr { Heading $1 $2 }
        --  | templ RootExpr templend %prec BLOCK { TemplateBlock $1 $2 }
         | templ %prec COMMAND { Template $1 }
         | "- " Expr "\n" %prec DATA { UnorderedList [$2] } -- TODO: Concatenate all together at end
         | "n. " Expr "\n" %prec DATA { OrderedList [$2] }
         | Expr { Body $1 }
         | RootExpr RootExpr %prec CONCAT { case ($1, $2) of -- Rules for concatenating root expressions
                                (RootSeq res1, RootSeq res2) -> RootSeq (res1 ++ res2)
                                (re1, RootSeq res2) -> RootSeq (re1 : res2)
                                (RootSeq res1, re2) -> RootSeq (res1 ++ [re2])
                                (OrderedList es1, OrderedList es2) -> OrderedList (es1 ++ es2)
                                (UnorderedList es1, UnorderedList es2) -> UnorderedList (es1 ++ es2)
                                _ -> RootSeq [$1, $2]
                                } 
         | "\n" { NewLine }

Expr : "**" Expr "**" %prec FORMAT { Bold $2 }
     | "*" Expr "*" %prec FORMAT { Italic $2 }
     | "[" Expr "]" "(" Expr ")" %prec FORMAT { Hyperlink $2 $5 } -- This allows non-text exprs, but is unfortunately unavoidable atm
     | text %prec DATA { Text $1 }
     | Expr Expr %prec CONCAT { case ($1, $2) of -- Rules for concatenating expressions
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


main = do
  s <- getContents
  print s
  print $ alexScanTokens s
  print $ parseMd $ (alexScanTokens s)
}