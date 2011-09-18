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
  allocateBlocksNear,
  freeBlocks,
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

-- | Finds a number of free blocks, allocates them, and returns the
-- allocated blocks.
allocateBlocks :: (Device m h, Integral i)
               => h
               -> Header
               -> i           -- ^ Number of blocks to allocate.
               -> m [Address] -- ^ A list of allocated blocks.
allocateBlocks h hdr c = readSBMap h hdr (0, sbs - 1) >>= a c
  where sbs = fromIntegral (superBlocks hdr)
        sbf = fromIntegral (superFactor hdr)
        spread = 16
        a c pool
          | c < 1               = return []
          | all (== SFull) pool = return []
          | otherwise =
            let em = find ((== SEmpty).fst) (pool `zip` [0..])
            in  case em of
                  Just (_,sb)
                    | c >= sbf ->
                      -- we've found an empty superblock, and we can fill it.
                      do writeSBState h hdr sb SFull
                         bmpSet h (blockMapBL hdr) (sbf *  sb
                                                   ,sbf * (sb+1) - 1)
                         let (p1,p2) = genericSplitAt (sb) pool
                             pool'   = p1 ++ (SFull : tail p2)
                         a' <- a (c - sbf) pool'
                         return $ map (indexToAddress hdr) [sbf *  sb ..
                                                            sbf * (sb+1) - 1] ++ a'
                    | otherwise ->
                      -- we've found an empty superblock, and we can start putting stuff in it.
                      do writeSBState h hdr sb SAvailable
                         let sbb  = sbf * sb
                             sbb' = sbb + c - 1
                         bmpSet h (blockMapBL hdr) (sbb, sbb')
                         return $ map (indexToAddress hdr) [sbb .. sbb']
                  Nothing ->
                    -- TODO: Search per-superblock instead of the whole block space.
                    do m <- bmpRead h (blockMapBL hdr) (0, maxBlocks hdr)
                       let bhs = map (\l -> (snd (head l), snd (last l))) .
                                 filter (\l -> fst (head l) == False) .
                                 groupBy (\(a,_) (b,_) -> a == b) $
                                 m `zip` [0..] -- block holes
                           phs = map (\(x,y)->(x+16,y-16)) $
                                 filter (\(x,y) -> y - x >= spread*2) bhs -- padded block holes
                           uhs = filter (\(x,y) -> y - x <  spread*2) bhs -- block holes without padding
                           al _          0 = return []
                           al []         _ = return []
                           al ((x,y):rs) b = let ls = minimum [y - x + 1, c]
                                                 am = (x, x + ls - 1)
                                             in  do bmpSet h (blockMapBL hdr) am
                                                    (map (indexToAddress hdr) [fst am..snd am] ++) <$>
                                                     al rs (b-ls)
                       adrs <- al (phs++uhs) c
                       mapM_ (rescanSB h hdr) (nub $ map (fst.toSB_R hdr) adrs)
                       return adrs

-- | Similar to 'allocateBlocks', except it tries to find blocks near
-- a reference block, if possible.
allocateBlocksNear :: (Device m h, Integral i)
                   => h
                   -> Header
                   -> Address      -- ^ Block to attempt to allocate more blocks near.
                                   -- Often the last block in a chain.
                   -> i            -- ^ Number of blocks to allocate.
                   -> m [Address]  -- ^ A list of allocated blocks.
allocateBlocksNear = undefined

-- | Marks a range of blocks on the allocation table as free.
freeBlocks :: Device m h
           => h
           -> Header
           -> (Address, Address)  -- ^ The range of blocks to free.
           -> m ()
freeBlocks = undefined

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

data SBState = SEmpty | SAvailable | SFull deriving (Show, Eq)

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
  do (case st of { SEmpty -> bmpClearAt ; _ -> bmpSetAt   }) h (blockMapSE hdr) i
     (case st of { SFull  -> bmpSetAt   ; _ -> bmpClearAt }) h (blockMapSF hdr) i

rescanSB :: (Device m h, Integral i) => h -> Header -> i -> m ()
rescanSB h hdr i =
  do bs <- bmpRead h (blockMapBL hdr) ( i    * fromIntegral (superFactor hdr)
                                      ,(i+1) * fromIntegral (superFactor hdr) - 1)
     writeSBState h hdr i $
       if True `elem` bs
          then if False `elem` bs
                  then SAvailable
                  else SFull
          else SEmpty