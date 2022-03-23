{-# LANGUAGE FlexibleContexts #-}
module TemplateLang.Command where

import TemplateLang.Values
import TemplateLang.Boolean

import Data.Bifunctor
import Data.Data (Typeable)
import qualified Data.Map as M
import qualified Prelude as P
import Prelude hiding (not)

data MetaCommand = If Bool | IfVar String | 
  Insert TValue | InsertVar String | 
  DocSetting String TValue | DocSettings TData |
  -- ImportQ = ModuleName QualifiedName
  LoadHsFile String | Import String | ImportQ String String
  deriving (Read, Show, Typeable)

tIf :: (ToBool a) => a -> MetaCommand
tIf = If . toBool

tIfNot :: (ToBool a) => a -> MetaCommand
tIfNot = If . not

tDocSetting :: ToTValue a => String -> a -> MetaCommand
tDocSetting str = DocSetting str . toTValue

tDocSettings :: ToTValue a => [(String, a)] -> MetaCommand
tDocSettings tdata = DocSettings $ M.fromList $ map (second toTValue) tdata
