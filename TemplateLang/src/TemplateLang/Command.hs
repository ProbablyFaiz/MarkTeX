{-# LANGUAGE FlexibleContexts #-}
module TemplateLang.Command where

import TemplateLang.Values
import TemplateLang.Boolean

import Data.Bifunctor
import Data.Data (Typeable)
import qualified Data.Map as M
import qualified Prelude as P
import Prelude hiding (not)

<<<<<<< HEAD
data MetaCommand = If Bool | IfVar String | Insert TValue | InsertVar String | SetVar String TValue | For String TValue | While Bool
=======
data MetaCommand = If TValue | IfVar String | 
  Insert TValue | InsertVar String | 
  DocSetting String TValue | DocSettings TData |
  -- ImportQ = ModuleName QualifiedName
  LoadHsFile String | Import String | ImportQ String String |
  SetVar String TValue | For String TValue | While Bool
>>>>>>> 2602537217de0a27e33d81fc42de2cbbb02de200
  deriving (Read, Show, Typeable)

tIf :: (ToTValue a) => a -> MetaCommand
tIf = If . toTValue

tIfNot :: (ToTValue a) => a -> MetaCommand
tIfNot = If . toTValue . not . toTValue 

tDocSetting :: ToTValue a => String -> a -> MetaCommand
tDocSetting str = DocSetting str . toTValue

tDocSettings :: ToTValue a => [(String, a)] -> MetaCommand
tDocSettings tdata = DocSettings $ M.fromList $ map (second toTValue) tdata

tSet :: ToTValue a => String -> a -> MetaCommand
tSet str = SetVar str . toTValue

tFor :: ToTValue a => String -> a -> MetaCommand
tFor str = For str . toTValue
