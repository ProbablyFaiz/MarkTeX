{-# OPTIONS_GHC -w #-}
module Parser (main, parseTokens) where

import Language
import Lexer (alexScanTokens)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,136) ([32704,4099,0,12344,65280,13,0,0,8192,0,8,0,0,0,12344,3584,12,0,57328,14336,112,3086,896,57351,448,32768,65280,32783,775,49632,12288,0,10,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,32768,0,32,2,128,8192,256,0,512,32784,1024,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTokens","RootExpr","Expr","PlainText","heading","\"**\"","\"*\"","text","\"\\n\"","\"- \"","\"n. \"","templ","tblockstart","tblockend","\"![\"","\"[\"","\"]\"","\"(\"","\")\"","%eof"]
        bit_start = st Prelude.* 22
        bit_end = (st Prelude.+ 1) Prelude.* 22
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..21]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (7) = happyShift action_2
action_0 (8) = happyShift action_6
action_0 (9) = happyShift action_7
action_0 (10) = happyShift action_8
action_0 (11) = happyShift action_9
action_0 (12) = happyShift action_10
action_0 (13) = happyShift action_11
action_0 (14) = happyShift action_12
action_0 (15) = happyShift action_13
action_0 (17) = happyShift action_14
action_0 (18) = happyShift action_15
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (8) = happyShift action_6
action_2 (9) = happyShift action_7
action_2 (10) = happyShift action_8
action_2 (17) = happyShift action_14
action_2 (18) = happyShift action_15
action_2 (5) = happyGoto action_27
action_2 (6) = happyGoto action_5
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (7) = happyShift action_2
action_3 (8) = happyShift action_6
action_3 (9) = happyShift action_7
action_3 (10) = happyShift action_8
action_3 (11) = happyShift action_9
action_3 (12) = happyShift action_10
action_3 (13) = happyShift action_11
action_3 (14) = happyShift action_12
action_3 (15) = happyShift action_13
action_3 (17) = happyShift action_14
action_3 (18) = happyShift action_15
action_3 (22) = happyAccept
action_3 (4) = happyGoto action_26
action_3 (5) = happyGoto action_4
action_3 (6) = happyGoto action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (8) = happyShift action_6
action_4 (9) = happyShift action_7
action_4 (10) = happyShift action_8
action_4 (17) = happyShift action_14
action_4 (18) = happyShift action_15
action_4 (5) = happyGoto action_25
action_4 (6) = happyGoto action_5
action_4 _ = happyReduce_6

action_5 (10) = happyShift action_8
action_5 (6) = happyGoto action_24
action_5 _ = happyReduce_14

action_6 (10) = happyShift action_8
action_6 (6) = happyGoto action_23
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (10) = happyShift action_8
action_7 (6) = happyGoto action_22
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_16

action_9 _ = happyReduce_8

action_10 (8) = happyShift action_6
action_10 (9) = happyShift action_7
action_10 (10) = happyShift action_8
action_10 (17) = happyShift action_14
action_10 (18) = happyShift action_15
action_10 (5) = happyGoto action_21
action_10 (6) = happyGoto action_5
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (8) = happyShift action_6
action_11 (9) = happyShift action_7
action_11 (10) = happyShift action_8
action_11 (17) = happyShift action_14
action_11 (18) = happyShift action_15
action_11 (5) = happyGoto action_20
action_11 (6) = happyGoto action_5
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_2

action_13 (7) = happyShift action_2
action_13 (8) = happyShift action_6
action_13 (9) = happyShift action_7
action_13 (10) = happyShift action_8
action_13 (11) = happyShift action_9
action_13 (12) = happyShift action_10
action_13 (13) = happyShift action_11
action_13 (14) = happyShift action_12
action_13 (15) = happyShift action_13
action_13 (17) = happyShift action_14
action_13 (18) = happyShift action_15
action_13 (4) = happyGoto action_19
action_13 (5) = happyGoto action_4
action_13 (6) = happyGoto action_5
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (8) = happyShift action_6
action_14 (9) = happyShift action_7
action_14 (10) = happyShift action_8
action_14 (17) = happyShift action_14
action_14 (18) = happyShift action_15
action_14 (19) = happyShift action_18
action_14 (5) = happyGoto action_17
action_14 (6) = happyGoto action_5
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (8) = happyShift action_6
action_15 (9) = happyShift action_7
action_15 (10) = happyShift action_8
action_15 (17) = happyShift action_14
action_15 (18) = happyShift action_15
action_15 (5) = happyGoto action_16
action_15 (6) = happyGoto action_5
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (8) = happyShift action_6
action_16 (9) = happyShift action_7
action_16 (10) = happyShift action_8
action_16 (17) = happyShift action_14
action_16 (18) = happyShift action_15
action_16 (19) = happyShift action_35
action_16 (5) = happyGoto action_25
action_16 (6) = happyGoto action_5
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (8) = happyShift action_6
action_17 (9) = happyShift action_7
action_17 (10) = happyShift action_8
action_17 (17) = happyShift action_14
action_17 (18) = happyShift action_15
action_17 (19) = happyShift action_34
action_17 (5) = happyGoto action_25
action_17 (6) = happyGoto action_5
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (20) = happyShift action_33
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (7) = happyShift action_2
action_19 (8) = happyShift action_6
action_19 (9) = happyShift action_7
action_19 (10) = happyShift action_8
action_19 (11) = happyShift action_9
action_19 (12) = happyShift action_10
action_19 (13) = happyShift action_11
action_19 (14) = happyShift action_12
action_19 (15) = happyShift action_13
action_19 (16) = happyShift action_32
action_19 (17) = happyShift action_14
action_19 (18) = happyShift action_15
action_19 (4) = happyGoto action_26
action_19 (5) = happyGoto action_4
action_19 (6) = happyGoto action_5
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (8) = happyShift action_6
action_20 (9) = happyShift action_7
action_20 (10) = happyShift action_8
action_20 (11) = happyShift action_31
action_20 (17) = happyShift action_14
action_20 (18) = happyShift action_15
action_20 (5) = happyGoto action_25
action_20 (6) = happyGoto action_5
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (8) = happyShift action_6
action_21 (9) = happyShift action_7
action_21 (10) = happyShift action_8
action_21 (11) = happyShift action_30
action_21 (17) = happyShift action_14
action_21 (18) = happyShift action_15
action_21 (5) = happyGoto action_25
action_21 (6) = happyGoto action_5
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (9) = happyShift action_29
action_22 (10) = happyShift action_8
action_22 (6) = happyGoto action_24
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (8) = happyShift action_28
action_23 (10) = happyShift action_8
action_23 (6) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (10) = happyShift action_8
action_24 (6) = happyGoto action_24
action_24 _ = happyReduce_17

action_25 (8) = happyShift action_6
action_25 (9) = happyShift action_7
action_25 (10) = happyShift action_8
action_25 (17) = happyShift action_14
action_25 (18) = happyShift action_15
action_25 (5) = happyGoto action_25
action_25 (6) = happyGoto action_5
action_25 _ = happyReduce_15

action_26 (7) = happyShift action_2
action_26 (8) = happyShift action_6
action_26 (9) = happyShift action_7
action_26 (10) = happyShift action_8
action_26 (11) = happyShift action_9
action_26 (12) = happyShift action_10
action_26 (13) = happyShift action_11
action_26 (14) = happyShift action_12
action_26 (15) = happyShift action_13
action_26 (17) = happyShift action_14
action_26 (18) = happyShift action_15
action_26 (4) = happyGoto action_26
action_26 (5) = happyGoto action_4
action_26 (6) = happyGoto action_5
action_26 _ = happyReduce_7

action_27 (8) = happyShift action_6
action_27 (9) = happyShift action_7
action_27 (10) = happyShift action_8
action_27 (17) = happyShift action_14
action_27 (18) = happyShift action_15
action_27 (5) = happyGoto action_25
action_27 (6) = happyGoto action_5
action_27 _ = happyReduce_1

action_28 _ = happyReduce_9

action_29 _ = happyReduce_10

action_30 _ = happyReduce_4

action_31 _ = happyReduce_5

action_32 _ = happyReduce_3

action_33 (10) = happyShift action_8
action_33 (6) = happyGoto action_38
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (20) = happyShift action_37
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (20) = happyShift action_36
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (10) = happyShift action_8
action_36 (6) = happyGoto action_41
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (10) = happyShift action_8
action_37 (6) = happyGoto action_40
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (10) = happyShift action_8
action_38 (21) = happyShift action_39
action_38 (6) = happyGoto action_24
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_12

action_40 (10) = happyShift action_8
action_40 (21) = happyShift action_43
action_40 (6) = happyGoto action_24
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (10) = happyShift action_8
action_41 (21) = happyShift action_42
action_41 (6) = happyGoto action_24
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_13

action_43 _ = happyReduce_11

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (THeading happy_var_1))
	 =  HappyAbsSyn4
		 (Heading happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (TTemplate happy_var_1))
	 =  HappyAbsSyn4
		 (Template happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal (TTemplateBlockStart happy_var_1))
	 =  HappyAbsSyn4
		 (TemplateBlock happy_var_1 happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (UnorderedList [happy_var_2]
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (OrderedList [happy_var_2]
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  4 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Body happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (case (happy_var_1, happy_var_2) of
                (RootSeq res1, RootSeq res2) -> RootSeq (res1 ++ res2)
                (re1, RootSeq res2) -> RootSeq (re1 : res2)
                (RootSeq res1, re2) -> RootSeq (res1 ++ [re2])
                (OrderedList es1, OrderedList es2) -> OrderedList (es1 ++ es2)
                (UnorderedList es1, UnorderedList es2) -> UnorderedList (es1 ++ es2)
                _ -> RootSeq [happy_var_1, happy_var_2]
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn4
		 (NewLine
	)

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Bold (Text happy_var_2)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Italic (Text happy_var_2)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 5 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Image happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 5 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Image (Text "") happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 5 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Hyperlink happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  5 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Text happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (case (happy_var_1, happy_var_2) of
            (Seq es1, Seq es2) -> Seq (es1 ++ es2)
            (Seq es1, e2) -> Seq (es1 ++ [e2])
            (e1, Seq es2) -> Seq (e1 : es2)
            (Text s1, Text s2) -> Text (s1 ++ s2)
            _ -> Seq [happy_var_1, happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyTerminal (TText happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	THeading happy_dollar_dollar -> cont 7;
	TBoldDelimiter -> cont 8;
	TItalicDelimiter -> cont 9;
	TText happy_dollar_dollar -> cont 10;
	TNewLine -> cont 11;
	TUnorderedItemStart -> cont 12;
	TOrderedItemStart -> cont 13;
	TTemplate happy_dollar_dollar -> cont 14;
	TTemplateBlockStart happy_dollar_dollar -> cont 15;
	TTemplateBlockEnd -> cont 16;
	TImageStart -> cont 17;
	TLBracket -> cont 18;
	TRBracket -> cont 19;
	TLHyperlink -> cont 20;
	TRHyperlink -> cont 21;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 22 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseTokens tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
