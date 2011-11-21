-- | Functions for dealing with TPFS blocks and allocation. Blocks are
-- structured on disk as such in pseudo-C:
-- 
-- @
--     struct Block {
--       char['blockSize' - 16] content;
--       'Address'              next;
--     };
-- @
module System.TPFS.Block (
  -- * Primitive operations
  -- ** Reading
  nextBlock,
  nthBlock,
  readBlock,
  readBlocks,
  readBlocksFrom,
  -- ** Writing
  consBlock,
  writeBlock,
  createBlock,
  overwriteBlock,
  createBlocks,
  overwriteBlocks,
  -- * Block allocation
  {-
  allocateBlocks,
  allocateBlocksNear,
  freeBlocks,
  -}
  -- * High-level interface
  {-
  getChain,
  editChain,
  putChain,
  truncateChain,
  deleteChain,
  -}
  -- * Mathematical helper functions
  divBlocks
  ) where

import           Control.Applicative
import qualified Control.Exception as E
import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.List
import           System.TPFS.Address
import           System.TPFS.Bitmap
import           System.TPFS.Device
import           System.TPFS.Errors
import           System.TPFS.Filesystem
import           System.TPFS.Header

-- | Gets the next block in the chain, if there is one.
nextBlock :: Device m h
          => Filesystem m h
          -> Address            -- ^ The address of the block.
          -> m (Maybe Address)  -- ^ The address of the next block, if there is one.

nextBlock (Filesystem h hdr) a
  = do ea <- dGet h (a + fromIntegral (blockSize hdr - 16)) 16
       case decode ea of
         0  -> return $ Nothing
         a' -> return $ Just a'

-- | Sets the address of the next block in the chain.
-- 
-- NB: It doesn't check for 'Just 0', which would be equivalent to
-- Nothing.
consBlock :: Device m h
          => Filesystem m h
          -> Address        -- ^ The address of the block to cons.
          -> Maybe Address  -- ^ The address of the block to cons to, or 'Nothing' for @[]@.
          -> m ()

consBlock (Filesystem h hdr) a b
  = dPut h (a + fromIntegral (blockSize hdr - 16)) (encode $ maybe 0 id b)

-- | Attempts to get the nth block of a chain.
nthBlock :: (Device m h, Integral i)
         => Filesystem m h
         -> Address            -- ^ The block to start at.
         -> i                  -- ^ The block index from zero to find.
         -> m (Maybe Address)  -- ^ 'Nothing' if the index is outside
                               -- the list, or 'Just' the block address.

nthBlock _  a 0 = return $ Just a
nthBlock fs a i
  = do ma' <- nextBlock fs a
       maybe (return Nothing) (\ a' -> nthBlock fs a' (i-1)) ma'

-- | Reads a single block from the filesystem.
readBlock :: Device m h
          => Filesystem m h
          -> Address       -- ^ The address of the block to read from.
          -> m ByteString  -- ^ The contents of the block.

readBlock (Filesystem h hdr) a = dGet h a (blockSize hdr - 16)

-- | Lazily reads an entire block chain into a bytestring. When used
-- with the IO monad, this function might break referential
-- transparency due to interleaving.
readBlocks :: Device m h
           => Filesystem m h
           -> Address       -- ^ The address of the first block.
           -> m ByteString  -- ^ The collective contents until the end of the chain,
                            -- appended lazily.

readBlocks fs a
  = do s <- readBlock fs a
       n <- nextBlock fs a
       case n of
         Nothing -> return s
         Just a' -> do s' <- interleave $ readBlocks fs a'
                       return $ B.append s s'

-- | Like 'readBlocks', but travels to the first block before
-- reading. Should make things faster for reading only part of a
-- file. Throws 'EndOfChain' if the block chain isn't big enough to
-- reach the offset.
readBlocksFrom :: (Device m h, Integral i)
               => Filesystem m h
               -> Address       -- ^ Address of the first block from which to calculate the offset.
               -> i             -- ^ Byte offset to start reading from.
               -> m ByteString

readBlocksFrom fs@(Filesystem h hdr) a o
    | o `div` bcs == 0 = B.drop (fromIntegral o) `fmap` readBlocks fs a
    | otherwise = do ma' <- nextBlock fs a
                     case ma' of
                       Nothing -> E.throw EndOfChain
                       Just a' -> readBlocksFrom fs a' (o - bcs)
  where bcs = fromIntegral (blockSize hdr - 16)

-- | Writes a bytestring to a block. The string will be truncated to
-- @blockSize hdr - 16@. If it is shorter than @blockSize hdr - 16@,
-- it will be padded with null bytes.
writeBlock :: Device m h
           => Filesystem m h
           -> Address     -- ^ The address to place the block at.
           -> ByteString  -- ^ The block content.
           -> m ()

writeBlock (Filesystem h hdr) a s =
  dPut h a $ truncateString (blockSize hdr - 16) s

-- | Combines 'writeBlock' and 'consBlock'.
createBlock :: Device m h
            => Filesystem m h
            -> Address                      -- ^ The address to write the block at.
            -> (ByteString, Maybe Address)  -- ^ The block content and next block address.
            -> m ()

createBlock fs a (s,n) = do writeBlock fs a s
                            consBlock  fs a n

-- | Writes "inside" a block at an offset. The string will be
-- truncated to a maximum length (@blockSize hdr - 16@), but if it is
-- smaller than that, it won't be padded (in contrast to
-- 'writeBlock').
overwriteBlock :: (Device m h, Integral i)
               => Filesystem m h
               -> Address       -- ^ The address of the block.
               -> i             -- ^ The byte offset relative to the start of the block.
               -> ByteString    -- ^ A string to insert into the block.
               -> m ByteString  -- ^ The overflow -- that is, the
                                -- remainder of the string too large
                                -- to fit within the block.

overwriteBlock (Filesystem h hdr) a o s
  = do dPut h (a + fromIntegral o) as
       return bs
  where (as,bs) = B.splitAt (fromIntegral (blockSize hdr - 16) - fromIntegral o) s

-- | Uses a given list of addresses to split a string into blocks and
-- write them to the addresses. 'OutOfSpace' will be raised if the
-- number of addresses in the list is not sufficient to store the
-- string.
createBlocks :: Device m h
             => Filesystem m h
             -> [Address]   -- ^ A list of addresses to put blocks in, in order of input.
             -> ByteString  -- ^ The string to fill the blocks with.
             -> m ()

createBlocks fs@(Filesystem h hdr) adrs s
    | B.null s            = return ()
    | alen == blen        = f adrs (chopString (blockSize hdr - 16) s)
    | otherwise           = E.throw OutOfSpace
  where f (a:   _) (c:[]) = createBlock fs a (c,Nothing)
        f (a:n:as) (c:cs) = createBlock fs a (c,Just  n) >> f (n:as) cs
        blen              = B.length s `divBlocks` hdr
        alen              = genericLength $ genericTake blen adrs
                              -- not pretty, but it works with infinite lists.

-- | Writes inside a block chain at a byte offset. It won't create new
-- blocks, so if there are some bytes that couldn't be written within
-- the block chain, they will be returned.
overwriteBlocks :: (Device m h, Integral i)
                => Filesystem m h
                -> Address       -- ^ The base block address.
                -> i             -- ^ The byte offset relative to the base
                                 -- block address, within block contents.
                -> ByteString    -- ^ The string to write inside the block
                                 -- contents, starting from the offset.
                -> m ByteString  -- ^ The remainder of the string that couldn't
                                 -- fit within the block chain.

overwriteBlocks fs@(Filesystem h hdr) a o s =
  do ma' <- nthBlock fs a (o `div` bsc)
     case ma' of
       Nothing -> return s
       Just a' -> f a' (o `mod` bsc) s
  where bsc     = fromIntegral (blockSize hdr - 16)
        f a o s = do s' <- overwriteBlock fs a o s
                     if B.null s'
                        then return B.empty
                        else do ma' <- nextBlock fs a
                                case ma' of
                                  Nothing -> return s'
                                  Just a' -> f a' 0 s'

truncateString l s = head (chopString l s)

chopString :: Integral l => l -> ByteString -> [ByteString]

chopString l s =
  case B.length s `compare` len of
    EQ -> [s]
    LT -> [s `B.append` B.replicate (len - B.length s) 0]
    GT -> let (a,b) = B.splitAt len s in a : chopString len b
  where len = fromIntegral l

-- | Divides a number of bytes into the required number of blocks
-- according to a filesystem header.
divBlocks :: Integral i => i -> Header -> i

i `divBlocks` hdr
    | r == 0    = q
    | otherwise = q + 1
  where (q, r)  = i `quotRem` fromIntegral (blockSize hdr - 16)