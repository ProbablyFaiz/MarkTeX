{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module MarkTeX.TemplateLang.Command where

import MarkTeX.TemplateLang.Values
import MarkTeX.TemplateLang.Boolean

import Data.Bifunctor
import Data.Data (Typeable)
import qualified Data.Map as M
import Prelude hiding (not)
import MarkTeX.TemplateLang.Expression

data MetaCommand = 
  If TValue | IfVar String | 
  Insert TValue | InsertVar String | InsertExpr Expr |
  SetVar String TValue | 
  DocSetting String TValue | DocSettings TData |
  -- ImportQ = ModuleName QualifiedName
  LoadHsFile String | Import String | ImportQ String String |
  Include String | IncludeWith String TData | 
  ReadJson String | ReadJsonQ String String |
  For String TValue | While TValue
  -- TODO add LateX commands (with options)
  -- TODO add a nice shortcut function with the above for colors
  deriving (Read, Show, Typeable)

ifTrue :: (ToTValue a) => a -> MetaCommand
ifTrue = If . toTValue
 
ifFalse :: (ToTValue a) => a -> MetaCommand
ifFalse = If . toTValue . not . toTValue 

insert :: (ToTValue a) => a -> MetaCommand
insert = Insert . toTValue

docSetting :: ToTValue a => String -> a -> MetaCommand
docSetting str = DocSetting str . toTValue

docSettings :: ToTValue a => [(String, a)] -> MetaCommand
docSettings tdata = DocSettings $ M.fromList $ map (second toTValue) tdata

set :: ToTValue a => String -> a -> MetaCommand
set str = SetVar str . toTValue

for :: ToTValue a => String -> a -> MetaCommand
for str = For str . toTValue

while :: ToTValue a => a -> MetaCommand
while = While . toTValue
