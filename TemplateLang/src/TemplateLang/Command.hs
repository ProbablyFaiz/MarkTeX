{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module TemplateLang.Command where

import TemplateLang.Values
import TemplateLang.Boolean

import Data.Bifunctor
import Data.Data (Typeable)
import qualified Data.Map as M
import qualified Prelude as P
import Prelude hiding (not)

data MetaCommand = If TValue | IfVar String | 
  Insert TValue | InsertVar String | 
  DocSetting String TValue | DocSettings TData |
  -- ImportQ = ModuleName QualifiedName
  LoadHsFile String | Import String | ImportQ String String |
  SetVar String TValue | For String TValue
  deriving (Read, Show, Typeable)

tIf :: (ToTValue a) => a -> MetaCommand
tIf = If . toTValue

tIfNot :: (ToTValue a) => a -> MetaCommand
tIfNot = If . toTValue . not . toTValue 

tInsert :: (ToTValue a) => a -> MetaCommand
tInsert = Insert . toTValue

tDocSetting :: ToTValue a => String -> a -> MetaCommand
tDocSetting str = DocSetting str . toTValue

tDocSettings :: ToTValue a => [(String, a)] -> MetaCommand
tDocSettings tdata = DocSettings $ M.fromList $ map (second toTValue) tdata

tSet :: ToTValue a => String -> a -> MetaCommand
tSet str = SetVar str . toTValue

tFor :: ToTValue a => String -> a -> MetaCommand
tFor str x = For str (toTValue x)
