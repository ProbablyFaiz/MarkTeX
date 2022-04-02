{
module MarkTeX.Parsing.Parser (main, parseTokens, parseMd) where

import MarkTeX.Parsing.Expression
import MarkTeX.Parsing.Lexer (alexScanTokens)
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
    templ       { TCommand $$ }
    tblockstart { TCommandBlockStart $$ }
    tblockend   { TCommandBlockEnd }
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
         | tblockstart RootExpr tblockend %prec COMMAND { CommandBlockCode $1 $2 }
         | "- " Expr "\n" %prec DATA { UnorderedList [$2] }
         | "n. " Expr "\n" %prec DATA { OrderedList [$2] }
         | Expr { Body $1 }
         | RootExpr RootExpr %prec CONCAT { RootSeq [$1, $2] }
         | "\n" { NewLine }

Expr : "**" SafeExpr "**" %prec FORMAT { Bold $2 }
     | "*" SafeExpr "*" %prec FORMAT { Italic $2 }
     | "![" Expr "]" "(" SafeExpr ")" %prec FORMAT { Image $2 $5 }
     | "![" "]" "(" SafeExpr ")" %prec FORMAT { Image (Text "") $4 } -- Allow images with no alt text
     | "[" Expr "]" "(" SafeExpr ")" %prec FORMAT { Hyperlink $2 $5 }
     | SafeExpr { $1 }
     | Expr Expr %prec CONCAT { Seq [$1, $2] }

SafeExpr : text %prec DATA { Text $1 }
          | templ %prec COMMAND { CommandCode $1 }
          | SafeExpr SafeExpr %prec CONCAT { Seq [$1, $2] }
{

parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts

optimizeRootExpr :: RootExpr -> RootExpr
optimizeRootExpr re = case re of
  RootSeq [re'] -> optimizeRootExpr re'
  RootSeq res -> RootSeq (concatRootExprs $ map optimizeRootExpr res)
  Body e -> Body (optimizeExpr e)
  Heading h e -> Heading h (optimizeExpr e)
  CommandBlockCode t e -> CommandBlockCode t (optimizeRootExpr e)
  UnorderedList es -> UnorderedList (map optimizeExpr es)
  OrderedList es -> OrderedList (map optimizeExpr es)
  _ -> re

concatRootExprs :: [RootExpr] -> [RootExpr]
concatRootExprs (x:y:xs) = case (x, y) of
    (OrderedList xList, OrderedList yList) -> concatRootExprs $ OrderedList (xList ++ yList) : xs
    (UnorderedList xList, UnorderedList yList) -> concatRootExprs $ UnorderedList (xList ++ yList) : xs
    (RootSeq xList, RootSeq yList) -> concatRootExprs $ RootSeq (xList ++ yList) : xs
    (RootSeq xList, re) -> concatRootExprs $ RootSeq (xList ++ [re]) : xs
    (re, RootSeq yList) -> concatRootExprs $ RootSeq (re : yList) : xs
    _ -> x : concatRootExprs (y : xs)
concatRootExprs xs = xs

optimizeExpr :: Expr -> Expr
optimizeExpr e = case e of
  Seq [e'] -> optimizeExpr e'
  Seq es -> Seq (concatExprs $ map optimizeExpr es)
  Bold e -> Bold (optimizeExpr e)
  Italic e -> Italic (optimizeExpr e)
  Image e1 e2 -> Image (optimizeExpr e1) (optimizeExpr e2)
  Hyperlink e1 e2 -> Hyperlink (optimizeExpr e1) (optimizeExpr e2)
  _ -> e

concatExprs :: [Expr] -> [Expr]
concatExprs es = case es of
    (x:y:rest) -> case (x, y) of
        (Seq xs, Seq ys) -> concatExprs $ Seq (concatExprs xs ++ concatExprs ys) : rest
        (Text xs, Text ys) -> concatExprs $ Text (xs ++ ys) : rest
        (Seq xs, e) -> concatExprs $ Seq (concatExprs xs ++ [e]) : rest
        (e, Seq ys) -> concatExprs $ Seq (e : concatExprs ys) : rest
        _ -> x : concatExprs (y : rest)
    _ -> es


-- Optimizes the root expression until no further optimizations are possible
fixRootExpr :: RootExpr -> RootExpr
fixRootExpr re = if re == re' then re else fixRootExpr re'
  where re' = optimizeRootExpr re


parseMd :: String -> RootExpr
parseMd md = fixRootExpr $ parseTokens $ alexScanTokens md


main = do
  s <- getContents
  print s
  print $ alexScanTokens s
  print $ parseMd s
}