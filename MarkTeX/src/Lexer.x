{
module Lexer (main, Token, alexScanTokens) where
}

-- TODO: Rewrite as monadic w/ startcodes so hyperlinks, templates etc. can be much less hacky
%wrapper "basic"

$digit = 0-9			-- digits
$text = [^$white\{\}]		-- text characters

tokens :-
  ^"#"{1,5}" "    { \s -> THeading $ length s }
  \*\*            { \s -> TBoldDelimiter }
  \*              { \s -> TItalicDelimiter }
  \(\"            { \s -> TLHyperlink }
  \"\)            { \s -> TRHyperlink }
  \[              { \s -> TLBracket }
  \]              { \s -> TRBracket }
  "{{" $white* "end" $white* "}}"      { \s -> TTemplateEnd }
  "{{".+"}}"      { \s -> TTemplate (let s' = drop 2 s in take (length s' - 2) s') }
  ^"- "           { \s -> TUnorderedItemStart }
  ^$digit". "     { \s -> TOrderedItemStart }
  \n              { \s -> TNewLine }
  $white          { \s -> TText s }
  $text+          { \s -> TText s } -- This rule should really be . but doing this now for output readability
{

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

main = do
  s <- getContents
  print s
  print (alexScanTokens s)
}