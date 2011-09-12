{-# LANGUAGE DeriveDataTypeable #-}

module System.TPFS.Errors (
  OutOfSpace(..),
  EndOfChain(..)
  ) where

import           Control.Exception (Exception)
import           Data.Typeable

-- | Thrown when there are not enough blocks available in the
-- filesystem to satisfy the requirements of a given operation.
data OutOfSpace = OutOfSpace deriving Typeable

instance Show OutOfSpace where
  showsPrec _ _ = showString "The target filesystem does not have enough free blocks left to perform this operation."

instance Exception OutOfSpace

-- | Thrown when the end of a block chain was reached before it was
-- expected to end.
data EndOfChain = EndOfChain deriving Typeable

instance Show EndOfChain where
  showsPrec _ _ = showString "Reached end of block chain prematurely."

instance Exception EndOfChain