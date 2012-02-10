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
  divBlocks,
  allocateBlocks,
  allocateBlock,
  freeBlocks,
  freeBlock,
  -- * Block arrays
  BlockArray(..),
  readBlockArray,
  writeBlockArray,
  linkBlockArray,
  -- * Extents
  allocateExtent,
  freeExtent
  ) where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad (replicateM)
import           Data.Array
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
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

--- Blocks ---

type BlockIndex = Word64

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

allocateBlocks = undefined

allocateBlock = undefined

freeBlocks = undefined

freeBlock = undefined

--- Block arrays ---

data BlockArray = BlockArray { blocks    :: Array Word (BlockIndex)
                             , nextArray :: BlockIndex
                             }

-- | Reads a 'BlockArray' object from disk.
--
-- Please note that this makes no attempt to check whether the object being
-- referenced is really a 'BlockArray'.
readBlockArray :: Device m h
               => h
               -> Header
               -> BlockIndex   -- ^ Index of the 'BlockArray' object to read.
               -> m BlockArray -- ^ The read information.

readBlockArray h hdr idx =
  do ars <- dGet h (blockIndexToAddress hdr idx) (blockSize hdr)
     let ar = runGet (replicateM (fromIntegral elc) $ getWord64le) ars
     return $ BlockArray { blocks    = listArray (0, fromIntegral elc-2) (init ar)
                         , nextArray = last ar
                         }
  where elc = blockSize hdr `quot` 8 -- Word64 is 8 bytes

-- | Writes a 'BlockArray' object to disk.
writeBlockArray :: Device m h
                => h
                -> Header
                -> BlockIndex
                -> BlockArray
                -> m ()

writeBlockArray h hdr idx ary = dPut h (blockIndexToAddress hdr idx) str
  where str     = runPut $ foldl put (pure ()) (elems $ blocks ary) >> putWord64le (nextArray ary)
        put m e = m >> putWord64le e

-- | Links a 'BlockArray' object with another 'BlockArray' object on disk,
-- without needing to read and replace the entire object.
linkBlockArray :: Device m h
               => h
               -> Header
               -> BlockIndex -- ^ The 'BlockArray' object to modify.
               -> BlockIndex -- ^ The destination for the 'nextArray' field.
               -> m ()

linkBlockArray h hdr a b = dPut h adr $ runPut $ putWord64le b
  where adr = blockIndexToAddress hdr a + fromIntegral ((quot (blockSize hdr) 8 - 1) * 8)

--- Extents ---

allocateExtent = undefined

freeExtent = undefined
