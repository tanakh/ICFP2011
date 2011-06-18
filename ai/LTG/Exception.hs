{-# Language DeriveDataTypeable #-}

module LTG.Exception (
  LTGError(..),
  ) where

import Control.Exception as E
import Data.Typeable

data LTGError = LTGError String
  deriving (Typeable, Show)

instance E.Exception LTGError
