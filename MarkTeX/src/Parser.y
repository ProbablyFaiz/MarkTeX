{
module Parser (main, parseTokens) where

import Language
import Lexer (alexScanTokens)
}

%name parseTokens
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
    tblockstart { TTemplateBlockStart $$ }
    tblockend   { TTemplateBlockEnd }
    "!["        { TImageStart }
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
         | templ %prec COMMAND { Template $1 }
         | tblockstart RootExpr tblockend %prec COMMAND { TemplateBlock $1 $2 }
         | "- " Expr "\n" %prec DATA { UnorderedList [$2] }
         | "n. " Expr "\n" %prec DATA { OrderedList [$2] }
         | Expr { Body $1 }
         | RootExpr RootExpr %prec CONCAT { 
            case ($1, $2) of
                (RootSeq res1, RootSeq res2) -> RootSeq (res1 ++ res2)
                (re1, RootSeq res2) -> RootSeq (re1 : res2)
                (RootSeq res1, re2) -> RootSeq (res1 ++ [re2])
                (OrderedList es1, OrderedList es2) -> OrderedList (es1 ++ es2)
                (UnorderedList es1, UnorderedList es2) -> UnorderedList (es1 ++ es2)
                _ -> RootSeq [$1, $2]
         }
         | "\n" { NewLine }

Expr : "**" PlainText "**" %prec FORMAT { Bold (Text $2) }
     | "*" PlainText "*" %prec FORMAT { Italic (Text $2) }
     | "![" Expr "]" "(" PlainText ")" %prec FORMAT { Image $2 $5 }
     | "![" "]" "(" PlainText ")" %prec FORMAT { Image (Text "") $4 } -- Allow images with no alt text
     | "[" Expr "]" "(" PlainText ")" %prec FORMAT { Hyperlink $2 $5 }
     | PlainText { Text $1 }
     | Expr Expr %prec CONCAT {
        case ($1, $2) of
            (Seq es1, Seq es2) -> Seq (es1 ++ es2)
            (Seq es1, e2) -> Seq (es1 ++ [e2])
            (e1, Seq es2) -> Seq (e1 : es2)
            (Text s1, Text s2) -> Text (s1 ++ s2)
            _ -> Seq [$1, $2]
        }

PlainText : text %prec DATA { $1 }
          | PlainText PlainText %prec CONCAT { $1 ++ $2 }
{

parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts


optimizeRootExpr :: RootExpr -> RootExpr
optimizeRootExpr re = case re of
  RootSeq res -> RootSeq (concatLists $ map optimizeRootExpr res)
  _ -> re


concatLists :: [RootExpr] -> [RootExpr]
concatLists (x:y:xs) = case (x, y) of
    (OrderedList xList, OrderedList yList) -> concatLists $ OrderedList (xList ++ yList) : xs
    (UnorderedList xList, UnorderedList yList) -> concatLists $ UnorderedList (xList ++ yList) : xs
    _ -> x : concatLists (y : xs)
concatLists xs = xs


parseMd :: String -> RootExpr
parseMd md = optimizeRootExpr $ parseTokens $ alexScanTokens md


main = do
  s <- getContents
  print s
  print $ alexScanTokens s
  print $ parseMd s
}