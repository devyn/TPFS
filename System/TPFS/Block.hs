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
import           System.TPFS.Bitmap
import           System.TPFS.Device
import           System.TPFS.Errors
import           System.TPFS.Filesystem
import           System.TPFS.Header

--- Blocks ---

-- | Divides a number of bytes into the required number of blocks
-- according to a filesystem header.
divBlocks :: Integral i => i -> Header -> i

i `divBlocks` hdr
    | r == 0    = q
    | otherwise = q + 1
  where (q, r)  = i `quotRem` fromIntegral (blockSize hdr - 16)

allocateBlocks = undefined

allocateBlock = undefined

freeBlocks = undefined

freeBlock = undefined

--- Block arrays ---

data BlockArray = BlockArray { blocks    :: Array Word Address
                             , nextArray :: Address
                             }

-- | Reads a 'BlockArray' object from disk.
--
-- Please note that this makes no attempt to check whether the object being
-- referenced is really a 'BlockArray'.
readBlockArray :: Device m h
               => Filesystem m h
               -> Address      -- ^ The address of the 'BlockArray' object to read.
               -> m BlockArray -- ^ The read information.

readBlockArray fs src =
  do ars <- dGet (fsHandle fs) src (blockSize $ fsHeader fs)
     let ar = runGet (replicateM (fromIntegral elc) $ getWord64le) ars
     return $ BlockArray { blocks    = listArray (0, fromIntegral elc-2) (init ar)
                         , nextArray = last ar
                         }
  where elc = blockSize (fsHeader fs) `quot` 8 -- Word64 is 8 bytes

-- | Writes a 'BlockArray' object to disk.
writeBlockArray :: Device m h
                => Filesystem m h
                -> Address    -- ^ The address of the block to write to.
                -> BlockArray -- ^ The 'BlockArray' object to write.
                -> m ()

writeBlockArray fs dst ary = dPut (fsHandle fs) dst str
  where str     = runPut $ foldl put (pure ()) (elems $ blocks ary)
                        >> putWord64le (nextArray ary)
        put m e = m >> putWord64le e

-- | Links a 'BlockArray' object with another 'BlockArray' object on disk,
-- without needing to read and replace the entire object.
linkBlockArray :: Device m h
               => Filesystem m h
               -> Address -- ^ The address of the 'BlockArray' object to modify.
               -> Address -- ^ The destination for the 'nextArray' field.
               -> m ()

linkBlockArray fs a b = dPut (fsHandle fs) adr . runPut $ putWord64le b
            where adr = a + blockSize (fsHeader fs) - 8

--- Extents ---

allocateExtent = undefined

freeExtent = undefined
