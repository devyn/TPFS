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
  allocateBlocks,
  freeBlocks,
  getFreeBlocks,
  getFreeBlocksNear,
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
import           System.TPFS.Header

-- | Gets the next block in the chain, if there is one.
nextBlock :: Device m h
          => h
          -> Header
          -> Address            -- ^ The address of the block.
          -> m (Maybe Address)  -- ^ The address of the next block, if there is one.
nextBlock h hdr a = do ea <- dGet h (a + fromIntegral (blockSize hdr - 16)) 16
                       case decode ea of
                         0  -> return $ Nothing
                         a' -> return $ Just a'

-- | Sets the address of the next block in the chain.
-- 
-- NB: It doesn't check for 'Just 0', which would be equivalent to
-- Nothing.
consBlock :: Device m h
          => h
          -> Header
          -> Address        -- ^ The address of the block to cons.
          -> Maybe Address  -- ^ The address of the block to cons to, or 'Nothing' for @[]@.
          -> m ()
consBlock h hdr a b = dPut h (a + fromIntegral (blockSize hdr - 16)) (encode $ maybe 0 id b)

-- | Attempts to get the nth block of a chain.
nthBlock :: (Device m h, Integral i)
         => h
         -> Header
         -> Address            -- ^ The block to start at.
         -> i                  -- ^ The block index from zero to find.
         -> m (Maybe Address)  -- ^ 'Nothing' if the index is outside
                               -- the list, or 'Just' the block address.
nthBlock h hdr a 0 = return $ Just a
nthBlock h hdr a i = do ma' <- nextBlock h hdr a
                        maybe (return Nothing) (\ a' -> nthBlock h hdr a' (i-1)) ma'

-- | Reads a single block from the filesystem.
readBlock :: Device m h
          => h
          -> Header
          -> Address       -- ^ The address of the block to read from.
          -> m ByteString  -- ^ The contents of the block.
readBlock h hdr a = dGet h a (blockSize hdr - 16)

-- | Lazily reads an entire block chain into a bytestring. When used
-- with the IO monad, this function might break referential
-- transparency due to interleaving.
readBlocks :: Device m h
           => h
           -> Header
           -> Address       -- ^ The address of the first block.
           -> m ByteString  -- ^ The collective contents until the end of the chain,
                            -- appended lazily.
readBlocks h hdr a = do s <- readBlock h hdr a
                        n <- nextBlock h hdr a
                        case n of
                          Nothing -> return s
                          Just a' -> do s' <- interleave $ readBlocks h hdr a'
                                        return $ B.append s s'

-- | Like 'readBlocks', but travels to the first block before
-- reading. Should make things faster for reading only part of a
-- file. Throws 'EndOfChain' if the block chain isn't big enough to
-- reach the offset.
readBlocksFrom :: (Device m h, Integral i)
               => h
               -> Header
               -> Address       -- ^ Address of the first block from which to calculate the offset.
               -> i             -- ^ Byte offset to start reading from.
               -> m ByteString
readBlocksFrom h hdr a o
    | o `div` bcs == 0 = B.drop (fromIntegral o) `fmap` readBlocks h hdr a
    | otherwise = do ma' <- nextBlock h hdr a
                     case ma' of
                       Nothing -> E.throw EndOfChain
                       Just a' -> readBlocksFrom h hdr a' (o - bcs)
  where bcs = fromIntegral (blockSize hdr - 16)

-- | Writes a bytestring to a block. The string will be truncated to
-- @blockSize hdr - 16@. If it is shorter than @blockSize hdr - 16@,
-- it will be padded with null bytes.
writeBlock :: Device m h
           => h
           -> Header      -- ^ The filesystem header.
           -> Address     -- ^ The address to place the block at.
           -> ByteString  -- ^ The block content.
           -> m ()
writeBlock h hdr a s =
  dPut h a $ truncateString (blockSize hdr - 16) s

-- | Combines 'writeBlock' and 'consBlock'.
createBlock :: Device m h
            => h
            -> Header
            -> Address                      -- ^ The address to write the block at.
            -> (ByteString, Maybe Address)  -- ^ The block content and next block address.
            -> m ()
createBlock h hdr a (s,n) = do writeBlock h hdr a s
                               consBlock  h hdr a n

-- | Writes "inside" a block at an offset. The string will be
-- truncated to a maximum length (@blockSize hdr - 16@), but if it is
-- smaller than that, it won't be padded (in contrast to
-- 'writeBlock').
overwriteBlock :: (Device m h, Integral i)
               => h
               -> Header
               -> Address       -- ^ The address of the block.
               -> i             -- ^ The byte offset relative to the start of the block.
               -> ByteString    -- ^ A string to insert into the block.
               -> m ByteString  -- ^ The overflow -- that is, the
                                -- remainder of the string too large
                                -- to fit within the block.
overwriteBlock h hdr a o s =
  do let (as,bs) = B.splitAt (fromIntegral (blockSize hdr - 16) - fromIntegral o) s
     dPut h (a + fromIntegral o) as
     return bs

-- | Uses a given list of addresses to split a string into blocks and
-- write them to the addresses. 'OutOfSpace' will be raised if the
-- number of addresses in the list is not sufficient to store the
-- string.
createBlocks :: Device m h
             => h
             -> Header
             -> [Address]   -- ^ A list of addresses to put blocks in, in order of input.
             -> ByteString  -- ^ The string to fill the blocks with.
             -> m ()
createBlocks h hdr adrs s
    | B.null s            = return ()
    | alen == blen        = f adrs (chopString (blockSize hdr - 16) s)
    | otherwise           = E.throw OutOfSpace
  where f (a:   _) (c:[]) = createBlock h hdr a (c,Nothing)
        f (a:n:as) (c:cs) = createBlock h hdr a (c,Just  n) >> f (n:as) cs
        blen              = B.length s `divBlocks` hdr
        alen              = genericLength $ genericTake blen adrs
                              -- not pretty, but it works with infinite lists.

-- | Writes inside a block chain at a byte offset. It won't create new
-- blocks, so if there are some bytes that couldn't be written within
-- the block chain, they will be returned.
overwriteBlocks :: (Device m h, Integral i)
                => h
                -> Header
                -> Address       -- ^ The base block address.
                -> i             -- ^ The byte offset relative to the base
                                 -- block address, within block contents.
                -> ByteString    -- ^ The string to write inside the block
                                 -- contents, starting from the offset.
                -> m ByteString  -- ^ The remainder of the string that couldn't
                                 -- fit within the block chain.
overwriteBlocks h hdr a o s =
  do ma' <- nthBlock h hdr a (o `div` bsc)
     case ma' of
       Nothing -> return s
       Just a' -> f a' (o `mod` bsc) s
  where bsc     = fromIntegral (blockSize hdr - 16)
        f a o s = do s' <- overwriteBlock h hdr a o s
                     if B.null s'
                        then return B.empty
                        else do ma' <- nextBlock h hdr a
                                case ma' of
                                  Nothing -> return s'
                                  Just a' -> f a' 0 s'

-- | Marks a list of blocks on the allocation table as allocated.
allocateBlocks :: Device m h
               => h
               -> Header
               -> [Address]  -- ^ A list of blocks to allocate, as absolute addresses to each block.
               -> m ()
allocateBlocks h hdr adrs = balloc bmpSet

-- | Marks a list of blocks on the allocation table as free.
freeBlocks :: Device m h
           => h
           -> Header
           -> [Address]  -- ^ A list of blocks to free, as absolute addresses to each block.
           -> m ()
freeBlocks = balloc bmpClear

-- DRYs up freeBlocks and allocateBlocks
balloc dobit h hdr adrs =
     do mapM_ (a . sort) ps
        mapM_ (rescanSB h hdr) $ nub $ map (fst . head) ps
  where ps   = groupBy g $ map (toSB_R hdr) (sort adrs)
        g (sb1,r1) (sb2,r2) = sb1 == sb2 && ((r2 - r1) `elem` [-1..1])
        a l  = let sb = fst (head l)
                   r1 = snd (head l)
                   r2 = snd (last l)
               in  dobit h (blockMapBL hdr) (sb * fromIntegral (superFactor hdr) + r1
                                            ,sb * fromIntegral (superFactor hdr) + r2)

getFreeBlocks :: Device m h
              => h
              -> Header
              -> m [Address]
getFreeBlocks = undefined

getFreeBlocksNear :: Device m h
                  => h
                  -> Header
                  -> Address
                  -> m [Address]
getFreeBlocksNear = undefined

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

indexToAddress :: Integral i => Header -> i -> Address
indexToAddress hdr i = blockOffset hdr + fromIntegral i * fromIntegral (blockSize hdr)

superScale = (^ 2) . superFactor

superFactor = blockSize

superBlocks hdr
    | r == 0    = q
    | otherwise = q + 1
  where (q, r)  = fromIntegral (maxBlocks hdr) `quotRem` fromIntegral (superFactor hdr)

toSB_R   :: Integral i => Header -> Address -> (i, i)
fromSB_R :: Integral i => Header -> (i, i)  -> Address

toSB_R   hdr a      = (fromIntegral sb, fromIntegral r `quot` fromIntegral (blockSize hdr))
  where      (sb,r) = (a - blockOffset hdr) `quotRem` fromIntegral (superScale hdr)
fromSB_R hdr (sb,r) = blockOffset hdr
                    + fromIntegral (sb * fromIntegral (superScale hdr))
                    + fromIntegral (r * fromIntegral (blockSize hdr))

data SBState = SEmpty | SAvailable | SFull

readSBMap :: (Device m h, Integral i) => h -> Header -> (i, i) -> m [SBState]
readSBMap h hdr (a,b) = do empty <- bmpRead h (blockMapSE hdr) (a,b)
                           full  <- bmpRead h (blockMapSF hdr) (a,b)
                           return $ zipWith f empty full
  where f True  True  = SFull
        f True  False = SAvailable
        f False False = SEmpty

searchSBMap :: (Device m h, Integral i) => h -> Header -> (i, i) -> SBState -> m [i]
searchSBMap h hdr (a,b) st =
  map snd . filter ((== st).fst) . (`zip` [a..]) <$> readSBMap h hdr (a,b)

writeSBState :: (Device m h, Integral i) => h -> Header -> i -> SBState -> m ()
writeSBState h hdr i st =
  do (case st of { SEmpty -> bmpClearAt, _ -> bmpSetAt   }) h (blockMapSE hdr) i
     (case st of { SFull  -> bmpSetAt,   _ -> bmpClearAt }) h (blockMapSF hdr) i

rescanSB :: (Device m h, Integral i) => h -> Header -> i -> m ()
rescanSB h hdr i =
  do bs <- bmpRead h (blockMapBL hdr) ( i    * superFactor hdr
                                      ,(i+1) * superFactor hdr - 1)
     writeSBState h hdr i $
       if True `elem` bs
          then if False `elem` bs
                  then SAvailable
                  else SFull
          else SEmpty