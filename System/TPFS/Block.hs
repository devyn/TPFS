-- | Functions for dealing with TPFS blocks.
--
-- This is not a high level interface; it only provides a few utility functions
-- for dealing with the blocks on disk themselves. It does not deal with
-- extents or anything like that. See 'System.TPFS.Extent'.
--
-- If you are looking for a high level interface, check out the modules for the
-- the objects you are trying to access, as TPFS blocks simply contain these
-- objects.
module System.TPFS.Block (
  -- * Indexing
  BlockIndex,
  blockIndexToAddress,
  addressToBlockIndex,
  addressToBlockIndexAndOffset,
  divBlocks,
  -- * Reading and writing
  readBlock,
  writeBlock
  ) where

import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           System.TPFS.Device
import           System.TPFS.Filesystem
import           System.TPFS.Header

--- Indexing ---

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

--- Reading and writing ---

-- | Reads an entire block into memory.
readBlock :: Device m h
          => Filesystem m h
          -> BlockIndex
          -> m ByteString

readBlock fs idx = dGet (fsHandle fs) (blockIndexToAddress (fsHeader fs) idx) (blockSize (fsHeader fs))

-- | Reads a section of a block into memory. Use sparingly: it is often faster
-- (especially on hard drives) to read an entire block and store it if you
-- think you'll be needing the same block shortly after.
--
-- This function does verify that the region that is to be read is indeed
-- within the block. The real offset is @offset `mod` 'blockSize' ('fsHeader'
-- fs)@, and the length will simply be truncated to the block boundary if it is
-- found to read outside of it.
readBlockSection :: Device m h
                 => Filesystem m h
                 -> BlockIndex
                 -> Word64         -- ^ The offset to start reading from.
                 -> Word64         -- ^ The length (in bytes) to read.
                 -> m ByteString

readBlockSection fs idx ofs len
      = dGet (fsHandle fs) (blockIndexToAddress (fsHeader fs) idx + ofs') len'
  where ofs' = ofs   `mod`  blockSize (fsHeader fs)
        len' | ofs' + len > blockSize (fsHeader fs) = blockSize (fsHeader fs) - ofs'
             | otherwise                            = len

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

-- | Writes a string within a block at a given offset. The string will be
-- truncated if it does not fit within the block boundaries, but it will never
-- be padded.
writeBlockSection :: Device m h
                  => Filesystem m h
                  -> BlockIndex
                  -> Word64
                  -> ByteString
                  -> m ()

writeBlockSection fs idx ofs str
      = dPut (fsHandle fs) (blockIndexToAddress (fsHeader fs) idx + ofs') str'
  where ofs' = ofs `mod` blockSize (fsHeader fs)
        str' = flip B.take str . fromIntegral $ blockSize (fsHeader fs) - ofs'
