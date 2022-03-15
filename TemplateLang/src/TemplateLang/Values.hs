{-# LANGUAGE FlexibleInstances #-}
module TemplateLang.Values where

import qualified Data.Map as M

import Data.Data (Typeable)
import Data.Functor.Const (Const)
import Data.Maybe (fromMaybe)

type IMap = M.Map String InputValue
data InputValue = IString String | INumber Float | IBool Bool | IList [InputValue] | IMap IMap | INull
  deriving (Read, Show, Typeable)

-- Utility functions
sameCons :: InputValue -> InputValue -> Bool
sameCons (IString _) (IString _) = True
sameCons (INumber _) (INumber _) = True
sameCons (IBool _)   (IBool _)   = True
sameCons (IList _)   (IList _)   = True
sameCons (IMap _)    (IMap _)    = True
sameCons INull   INull           = True
sameCons _       _               = False

toString :: InputValue -> String
toString (IString str)   = str
toString (INumber num)   = show num
toString (IBool bool)    = show bool
toString (IList vallist) = show vallist
toString (IMap imap)     = show imap
toString INull           = ""
