{
module MarkTeX.Parsing.Lexer where

import MarkTeX.Parsing.Expression

import Data.List (dropWhileEnd)
import Data.Char (isSpace)
}

%wrapper "monadUserState"

$digit     = 0-9
$text = [a-zA-z0-9\#\?\.\:\;\?\,\"\!\$\(\)\/]

tokens :-
<0>  ^"#"{1,5}" "    { pushToken $ THeading . subtract 1 . length }
<0>  \*\*            { pushToken $ const TBoldDelimiter }
<0>  \*              { pushToken $ const TItalicDelimiter }
<hyperlink>  \(      { pushToken $ const TLHyperlink }
<hyperlink>  \)      { endHyperlink }
<0>  \!\[            { pushToken $ const TImageStart }
<0>  \[              { pushToken $ const TLBracket }
<0>  \]              { startHyperlink }
<0>  "```"           { startCodeSnippet }
<snippet> "```"      { endCodeSnippet }
<0> "{{"             { startCommand }
<command> "}}"       { endCommand }
<snippet,command,commandBlock,hyperlink> . { appendStrBuf }
<snippet,command,commandBlock> \n { appendStrBuf } -- Needs to be given separately from . for some reason
<0> "{%"             { startCommandBlock }
<commandBlock> "%}"  { endCommandBlock }
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

alexEOF :: Alex ()
alexEOF = return ()

ignore input len = alexMonadScan

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

startCodeSnippet :: AlexAction ()
startCodeSnippet = \_ _ -> do
    alexSetStartCode snippet
    alexMonadScan

endCodeSnippet :: AlexAction ()
endCodeSnippet = \_ _ -> do
  alexSetStartCode 0
  modifyUserState $ \st -> st { strBuf = "", tokens = (TCodeSnippet $ strBuf st) : tokens st }
  alexMonadScan

startCommand :: AlexAction ()
startCommand = \_ _ -> do
  alexSetStartCode command
  alexMonadScan

endCommand :: AlexAction ()
endCommand = \_ _ -> do
  alexSetStartCode 0
  modifyUserState $ \st -> st { strBuf = "", tokens = (TCommand $ trim $ strBuf st) : tokens st }
  alexMonadScan

startCommandBlock :: AlexAction ()
startCommandBlock = \_ _ -> do
  alexSetStartCode commandBlock
  alexMonadScan

endCommandBlock :: AlexAction ()
endCommandBlock = \_ _ -> do
  alexSetStartCode 0
  modifyUserState (\st -> 
    let blockStr = trim $ strBuf st
        tkn = if blockStr == "end" then TCommandBlockEnd else TCommandBlockStart blockStr
    in  st { strBuf = "", tokens = tkn : tokens st }
    )
  alexMonadScan

startHyperlink :: AlexAction ()
startHyperlink = \_ _ -> do
  alexSetStartCode hyperlink
  modifyUserState $ \st -> st { tokens = TRBracket : tokens st }
  alexMonadScan

endHyperlink :: AlexAction ()
endHyperlink = \_ _ -> do
  alexSetStartCode 0
  modifyUserState $ \st -> st { strBuf = "", tokens = TRHyperlink : (TText $ trim $ strBuf st) : tokens st }
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

-- type AlexAction a = AlexInput -> Int -> Alex a
-- type AlexInput = (AlexPosn, Char, [Byte], String)

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
lexMd s = runAlexScan s >>= return . reverse . (TNewLine:) . tokens

-- main = getContents >>= print . runAlexScan
}