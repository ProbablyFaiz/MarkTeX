{-# LANGUAGE DeriveDataTypeable #-}

module MarkTeX.Parsing.Expression where

import Codec.Binary.UTF8.String (encode)
import Control.Monad.State (State, get)
import Data.Data (Data)
import Data.Word (Word8)

data Token
  = THeading Int
  | TImageStart
  | TLBracket
  | TRBracket
  | TLHyperlink
  | TRHyperlink
  | TText String
  | TBoldDelimiter
  | TItalicDelimiter
  | TUnorderedItemStart
  | TOrderedItemStart
  | TCommand String
  | TCommandBlockStart String
  | TCommandBlockEnd
  | TCodeSnippet String
  | TEof
  | TNewLine
  deriving (Show, Eq)

data RootExpr
  = Heading Int Expr
  | Body Expr
  | OrderedList [Expr]
  | UnorderedList [Expr]
  | NewLine
  | CommandBlockCode String RootExpr
  | RootSeq [RootExpr]
  deriving (Show, Eq, Data)

data Expr
  = Seq [Expr]
  | Text String
  | Bold Expr
  | Italic Expr
  | Hyperlink Expr Expr
  | Image Expr Expr
  | CommandCode String
  deriving (Show, Eq, Data)

-- The input: last character, unused bytes, remaining string
data AlexInput = AlexInput
  { aiprev :: Char,
    aibytes :: [Word8],
    airest :: String,
    ailineno :: Int
  }
  deriving (Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai =
  case aibytes ai of
    (b : bs) -> Just (b, ai {aibytes = bs})
    [] -> case airest ai of
      [] -> Nothing
      (c : cs) ->
        let n = ailineno ai
            n' = if c == '\n' then n + 1 else n
            (b : bs) = encode [c]
         in Just
              ( b,
                AlexInput
                  { aiprev = c,
                    aibytes = bs,
                    airest = cs,
                    ailineno = n'
                  }
              )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar AlexInput {aiprev = c} = c

data ParseState = ParseState
  { input :: AlexInput,
    lexSC :: Int, --Lexer start code
    stringBuf :: String --Temporary storage for strings
  }
  deriving (Show)

initialState :: String -> ParseState
initialState s =
  ParseState
    { input =
        AlexInput
          { aiprev = '\n',
            aibytes = [],
            airest = s,
            ailineno = 1
          },
      lexSC = 0,
      stringBuf = ""
    }


getLineNo::P Int
getLineNo = do
  s <- get
  return . ailineno . input $ s

-- Our Parser monad
type P a = State ParseState a
