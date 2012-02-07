-- | Functions for dealing with TPFS blocks, extents, and allocation.
module System.TPFS.Block (
  -- * Data structures
  BlockIndex,
  BlockArray(..),
  -- * Reading
  readBlockArray,
  bytePosToBlockIndexOffset,
  readBytesFromBlocks,
  -- * Writing
  writeBlockArray,
  linkBlockArray,
  -- * Allocation
  allocateExtent,
  allocateBlocks,
  freeExtent,
  freeBlocks,
  -- * Utility functions
  divBlocks,
  blockIndexToAddress,
  addressToBlockIndex
  ) where

import           Control.Applicative
import qualified Control.Exception as E
import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.List
import           Data.Word
import           System.TPFS.Address
import           System.TPFS.Bitmap
import           System.TPFS.Device
import           System.TPFS.Errors
import           System.TPFS.Filesystem
import           System.TPFS.Header

type BlockIndex = Word64

data BlockArray = BlockArray { blocks    :: Array Int (Maybe BlockIndex)
                             , nextArray :: Maybe BlockIndex
                             }

readBlockArray = undefined

bytePosToBlockIndexOffset = undefined

readBytesFromBlocks = undefined

writeBlockArray = undefined

linkBlockArray = undefined

allocateExtent = undefined

allocateBlocks = undefined

freeExtent = undefined

freeBlocks = undefined

-- | Divides a number of bytes into the required number of blocks
-- according to a filesystem header.
divBlocks :: Integral i => i -> Header -> i

i `divBlocks` hdr
    | r == 0    = q
    | otherwise = q + 1
  where (q, r)  = i `quotRem` fromIntegral (blockSize hdr - 16)

blockIndexToAddress = undefined

addressToBlockIndex = undefined