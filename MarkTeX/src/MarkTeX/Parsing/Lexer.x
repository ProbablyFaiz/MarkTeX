{
module MarkTeX.Parsing.Lexer where

import MarkTeX.Parsing.Expression
}

%wrapper "monadUserState"

$digit     = 0-9
$text = [a-zA-z0-9\#\?\.\:\;\?\,\"\!\$\(\)\/]
$nontemplatetag = [^\%]

-- Each token has a function on the RHS that is a function (String ->
-- Token). We wrap this function in the Alex monad with `pushToken`, which has
-- a result AlexAction ().

tokens :-
<0>  ^"#"{1,5}" "    { pushToken $ THeading . subtract 1 . length }
<0>  \*\*            { pushToken $ const TBoldDelimiter }
<0>  \*              { pushToken $ const TItalicDelimiter }
<0> \(\"             { pushToken $ const TLHyperlink }
<0>  \"\)            { pushToken $ const TRHyperlink }
<0>  \!\[            { pushToken $ const TImageStart }
<0>  \[              { pushToken $ const TLBracket }
<0>  \]              { pushToken $ const TRBracket }
<0>  "```"           { startCodeSnippet }
<snippet> .          { appendStrBuf }
<snippet> \n         { appendStrBuf }
<snippet> "```"      { endCodeSnippet }
<0>  "{{".+"}}"   { pushToken $ TCommand . (\s -> let s' = drop 2 s in take (length s' - 2) s') }
<0>  "{%" $white* "end" $white* "%}" { pushToken $ const TCommandBlockEnd }
<0>  "{%"$nontemplatetag+"%}"        { pushToken $ TCommandBlockStart . (\s -> let s' = drop 2 s in take (length s' - 2) s') }
<0>  ^"- "           { pushToken $ const TUnorderedItemStart }
<0>  ^$digit+". "    { pushToken $ const TOrderedItemStart }
<0>  \n              { pushToken $ const TNewLine }
<0>  .               { pushToken TText }
{

data AlexUserState = AlexUserState
                   {
                       tokens  :: [Token],
                       strBuf :: String
                   }
type ParseError    = String

alexEOF :: Alex ()
alexEOF = return ()

ignore input len = alexMonadScan

startCodeSnippet :: AlexAction ()
startCodeSnippet = \_ _ -> do
    alexSetStartCode snippet
    alexMonadScan

endCodeSnippet :: AlexAction ()
endCodeSnippet = \_ _ -> do
  alexSetStartCode 0
  modifyUserState $ \st -> st { strBuf = "", tokens = (TCodeSnippet $ strBuf st) : tokens st }
  alexMonadScan

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { tokens = []
                                   , strBuf = ""
                                   }

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let current = alex_ust s
                                     new     = f current
                                 in
                                   Right (s { alex_ust = new },())

getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s,alex_ust s)

pushToken :: (String -> Token) -> AlexAction ()
pushToken tokenizer =
  \(posn,prevChar,pending,s) len -> modifyUserState (push $ take len s) >> alexMonadScan
    where
       push :: String -> AlexUserState -> AlexUserState
       push s st = st { tokens = tokenizer s : tokens st }

appendStrBuf :: AlexAction ()
appendStrBuf = \(_,_,_,s) len -> modifyUserState (\st -> st { strBuf = strBuf st ++ take len s }) >> alexMonadScan

runAlexScan :: String -> Either ParseError AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

lexMd :: String -> Either ParseError [Token]
lexMd s = runAlexScan s >>= return . reverse . tokens

-- main = getContents >>= print . runAlexScan

}