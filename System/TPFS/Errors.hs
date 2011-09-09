{-# LANGUAGE DeriveDataTypeable #-}

module System.TPFS.Errors (OutOfSpace(..)) where

import           Control.Exception (Exception)
import           Data.Typeable

data OutOfSpace = OutOfSpace deriving Typeable

instance Show OutOfSpace where
  showsPrec _ _ = showString "The target filesystem does not have enough free blocks left to perform this operation."

instance Exception OutOfSpace