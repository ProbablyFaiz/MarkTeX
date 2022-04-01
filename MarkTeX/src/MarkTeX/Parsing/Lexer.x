{
module MarkTeX.Parsing.Lexer (main, alexScanTokens) where

import MarkTeX.Parsing.Expression
}

-- TODO: Rewrite as monadic w/ startcodes so hyperlinks, templates etc. can be much less hacky
%wrapper "basic"

$digit = 0-9			-- digits
$text = [a-zA-z0-9\#\?\.\:\;\?\,\"\!\$\(\)\/]		-- text characters

tokens :-
  ^"#"{1,5}" "    { \s -> THeading $ length s - 1 }
  \*\*            { \s -> TBoldDelimiter }
  \*              { \s -> TItalicDelimiter }
  \(\"            { \s -> TLHyperlink }
  \"\)            { \s -> TRHyperlink }
  \!\[            { \s -> TImageStart }
  \[              { \s -> TLBracket }
  \]              { \s -> TRBracket }
  "{{".+"}}"      { \s -> TCommand (let s' = drop 2 s in take (length s' - 2) s') }
  "{%" $white* "end" $white* "%}" { \s -> TCommandBlockEnd }
  "{%".+"%}"      { \s -> TCommandBlockStart (let s' = drop 2 s in take (length s' - 2) s') }
  ^"- "           { \s -> TUnorderedItemStart }
  ^$digit". "     { \s -> TOrderedItemStart }
  \n              { \s -> TNewLine }
  $white          { \s -> TText s }
  .               { \s -> TText s } -- This rule should really be . but doing this now for output readability
{

main = do
  s <- getContents
  print s
  print (alexScanTokens s)
}