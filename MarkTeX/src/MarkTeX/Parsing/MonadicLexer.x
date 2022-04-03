{
module MarkTeX.Parsing.MonadicLexer (lexer)  where

import MarkTeX.Parsing.Expression
import Control.Monad.State
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import System.IO.Unsafe (unsafePerformIO)
}

$digit = 0-9

tokens :-
<0,nl>  ^"#"{1,5}" "    { textTok (\s -> THeading $ length s - 1) }
<0,nl>  \*\*            { plainTok TBoldDelimiter }
<0,nl>  \*              { plainTok TItalicDelimiter }
<0,nl>  \(\"            { plainTok TLHyperlink }
<0,nl>  \"\)            { plainTok TRHyperlink }
<0,nl>  \!\[            { plainTok TImageStart }
<0,nl>  \[              { plainTok TLBracket }
<0,nl>  \]              { plainTok TRBracket }
<nl> "- "               { plainTok TUnorderedItemStart }
<nl> $digit+". "        { plainTok TOrderedItemStart }
<0,nl> \n               { newLine }
<0,nl> "{{"             { beginCommand }
<command> "}}"          { endCommand }
<0,nl> "{%"             { beginBlock }
<block> "%}"            { endBlock }
<0,nl> "```"            { beginCodeSnippet }
<codeSnippet> "```"     { endCodeSnippet }
<command,block,codeSnippet> .    { appendString }
<0,nl> .                { textTok TText }


{
type LexAction = Int -> String -> P (Maybe Token)

plainTok :: Token -> LexAction
plainTok t _ _ = do
    s <- get
    put s{lexSC = 0}
    seq (unsafePerformIO $ print ((Just t))) (return (Just t))

newLine :: LexAction
newLine _ _ = do
    s <- get
    put s{lexSC = nl}
    seq (unsafePerformIO $ print (Just TNewLine)) (return $ Just TNewLine)

textTok :: (String -> Token) -> LexAction
textTok cons _ s = seq (unsafePerformIO $ print (Just (cons s))) (return $ Just (cons s))

beginCodeSnippet :: LexAction
beginCodeSnippet _ _ = do
    s <- get
    put s{lexSC = codeSnippet}
    seq (unsafePerformIO $ print "") (return Nothing)

endCodeSnippet :: LexAction
endCodeSnippet _ _ = do
    s <- get
    put s{lexSC = 0, stringBuf = ""}
    seq (unsafePerformIO $ print (Just (TCodeSnippet (reverse (stringBuf s))))) (return $ Just (TCodeSnippet (reverse (stringBuf s))))

beginCommand :: LexAction
beginCommand _ _ = do
    s <- get
    put s{lexSC = command}
    seq (unsafePerformIO $ print "") (return Nothing)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

endCommand :: LexAction
endCommand _ _ = do
    s <- get
    put s{lexSC = 0, stringBuf = ""}
    let cmd = trim $ reverse $ stringBuf s
    seq (unsafePerformIO $ print (Just (TCommand cmd))) (return $ Just (TCommand cmd))

beginBlock :: LexAction
beginBlock _ _ = do
    s <- get
    put s{lexSC = block}
    seq (unsafePerformIO $ print "") (return Nothing)

endBlock :: LexAction
endBlock _ _ = do
    s <- get
    put s{lexSC = 0, stringBuf = ""}
    let cmd = trim $ reverse $ stringBuf s
    if cmd /= "end" then
        seq (unsafePerformIO $ print (Just (TCommandBlockStart cmd))) (return $ Just (TCommandBlockStart cmd))
    else
        seq (unsafePerformIO $ print (Just (TCommandBlockEnd))) (return $ Just (TCommandBlockEnd))

appendString :: LexAction
appendString _ (c:_) = do
  s <- get
  put s{stringBuf = c:(stringBuf s)}
  seq (unsafePerformIO $ print "") (return Nothing)
 
endString :: LexAction
endString _ _ = do
  s <- get
  let buf = stringBuf s
  put s{lexSC = 0, stringBuf = ""}
  seq (unsafePerformIO $ print (Just $ TText (reverse buf))) (return $ Just $ TText (reverse buf))
 
readToken :: P Token
readToken = do
  s <- get
  case alexScan (input s) (lexSC s) of
    AlexEOF -> return TEof
    AlexError inp' -> error $ "Lexical error on line "++(show $ ailineno inp')      
    AlexSkip inp' _ -> do    
      put s{input = inp'}
      readToken
    AlexToken inp' n act -> do 
      let (AlexInput{airest=buf}) = input s
      put s{input = inp'}
      res <- act n (take n buf)
      case res of
        Nothing -> readToken
        Just t -> return t

lexer :: (Token -> P a) -> P a
lexer cont = do
  tok <- readToken
  cont tok

main = do
  s <- getContents
  print s
}