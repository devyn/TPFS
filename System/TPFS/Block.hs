-- | Functions for dealing with TPFS blocks, extents, and allocation.
module System.TPFS.Block (
  -- * Data structures
  BlockIndex,
  BlockArray(..),
  -- * Reading
  readBlockArray,
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
import           Control.Monad (replicateM)
import           Data.Array
import           Data.Binary
import           Data.Binary.Get
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

data BlockArray = BlockArray { blocks    :: Array Word (BlockIndex)
                             , nextArray :: BlockIndex
                             }

readBlockArray :: Device m h
               => h
               -> Header
               -> BlockIndex
               -> m BlockArray

readBlockArray h hdr idx =
  do ars <- dGet h (blockIndexToAddress hdr idx) (blockSize hdr)
     let ar = runGet (replicateM (fromIntegral elc) $ getWord64le) ars
     return $ BlockArray { blocks    = listArray (0, fromIntegral elc-2) (init ar)
                         , nextArray = last ar
                         }
  where elc = blockSize hdr `quot` 8 -- Word64 is 8 bytes

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

blockIndexToAddress :: Header
                    -> BlockIndex
                    -> Address

blockIndexToAddress hdr idx = blockOffset hdr + fromIntegral (blockSize hdr) * fromIntegral (idx - 1)

addressToBlockIndex :: Header
                    -> Address
                    -> BlockIndex

addressToBlockIndex hdr a
      = toZero $ fromIntegral (quot (a - blockOffset hdr) (fromIntegral $ blockSize hdr)) + 1
  where toZero x | x < 1     = 0
                 | otherwise = x
