-- | Functions for dealing with TPFS blocks and extents.
--
-- This is not a high level interface; aside from providing the block array
-- structures, it does not deal with block chains. It does, however, provide
-- allocation, destruction, reading and writing of the individual objects
-- themselves.
--
-- If you are looking for a high level interface, check out the modules for the
-- the objects you are trying to access, as TPFS blocks simply contain these
-- objects. Different objects' blocks are often linked together in different
-- ways as well; some may be simple linked lists instead of dynamic arrays
-- ('BlockArray's) for space reasons.
module System.TPFS.Block (
  -- * Blocks
  BlockIndex,
  blockIndexToAddress,
  addressToBlockIndex,
  addressToBlockIndexAndOffset,
  divBlocks,
  readBlock,
  writeBlock,
  allocateBlocks,
  allocateBlock,
  freeBlocks,
  freeBlock,
  -- * Extents
  allocateExtent,
  freeExtent
  ) where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad (replicateM)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.List
import           Data.Word
import           System.TPFS.Bitmap
import           System.TPFS.Device
import           System.TPFS.Errors
import           System.TPFS.Filesystem
import           System.TPFS.Header

--- Blocks ---

type BlockIndex = Word64

-- | Converts a 'BlockIndex' to an 'Address' in the context of a filesystem.
blockIndexToAddress :: Header
                    -> BlockIndex
                    -> Address

blockIndexToAddress hdr idx = blockOffset hdr + fromIntegral (blockSize hdr) * fromIntegral (idx - 1)

-- | Converts an 'Address' to a 'Blockindex' in the context of a filesystem.
--
-- Note: This function will floor any addresses to the block boundary; it does
-- not preserve offsets. See 'addressToBlockIndexAndOffset'.
addressToBlockIndex :: Header
                    -> Address
                    -> BlockIndex

addressToBlockIndex hdr a
      = toZero $ fromIntegral (quot (a - blockOffset hdr) (fromIntegral $ blockSize hdr)) + 1
  where toZero x | x < 1     = 0
                 | otherwise = x

-- | Converts an 'Address' to a 'BlockIndex' and byte offset within the block
-- in the context of a filesystem.
--
-- Note: Undefined when the 'Address' points to somewhere before the beginning
-- of the block space.
addressToBlockIndexAndOffset :: Header
                             -> Address
                             -> (BlockIndex, Word64)

addressToBlockIndexAndOffset hdr a
      | a >= blockOffset hdr = (fromIntegral q + 1, fromIntegral r)
      | otherwise            = undefined
  where (q, r)               = quotRem (a - blockOffset hdr) (fromIntegral $ blockSize hdr)

-- | Divides a number of bytes into the required number of blocks
-- according to a filesystem header.
divBlocks :: Integral i => i -> Header -> i

i `divBlocks` hdr
    | r == 0    = q
    | otherwise = q + 1
  where (q, r)  = i `quotRem` fromIntegral (blockSize hdr - 16)

-- | Reads an entire block into memory.
readBlock :: Device m h
          => Filesystem m h
          -> BlockIndex
          -> m ByteString

readBlock fs idx = dGet (fsHandle fs) (blockIndexToAddress (fsHeader fs) idx) (blockSize (fsHeader fs))

-- | Replaces a block with the given string. The string is truncated/padded
-- with NULs to fit the filesystem's block size.
writeBlock :: Device m h
           => Filesystem m h
           -> BlockIndex
           -> ByteString
           -> m ()

writeBlock fs idx str = dPut (fsHandle fs) (blockIndexToAddress (fsHeader fs) idx) padstr
  where padstr | fromIntegral (B.length str) > blockSize (fsHeader fs)
                   = B.take (fromIntegral . blockSize $ fsHeader fs) str
               | fromIntegral (B.length str) < blockSize (fsHeader fs)
                   = B.append str . flip B.replicate 0 $ fromIntegral (blockSize $ fsHeader fs) - B.length str
               | otherwise
                   = str

allocateBlocks = undefined

allocateBlock = undefined

freeBlocks = undefined

freeBlock = undefined

--- Extents ---

allocateExtent = undefined

freeExtent = undefined
