module System.TPFS.Block where

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

-- | Reads a single block from the filesystem.
readBlock        :: Device m h
                 => h
                 -> Header
                 -> Address                        -- ^ The address of the block to read from.
                 -> m (ByteString, Maybe Address)  -- ^ The contents of the block, and a pointer to the
                                                   -- next block in the chain if there is one.
readBlock h hdr a = do c <- dGet h a (blockSize hdr)
                       let (bc, enn)
                                = B.splitAt (fromIntegral $ blockSize hdr - 16) c
                           next = decode enn
                           in return (bc, if next == 0
                                             then Nothing
                                             else Just next)

-- | Reads an entire block chain into a lazy bytestring.
readBlocks        :: Device m h => h -> Header -> Address -> m ByteString
readBlocks h hdr a = do (s,n) <- readBlock h hdr a
                        case n of
                          Nothing -> return s
                          Just a' -> fmap (s `B.append`) $ readBlocks h hdr a'

-- | Writes a bytestring to a block. The string will be truncated to
-- @blockSize hdr - 16@.
writeBlock :: Device m h
           => h
           -> Header                 -- ^ The filesystem header.
           -> Address                -- ^ The address to place the block at.
           -> (ByteString, Address)  -- ^ The block content and next block in the chain.
           -> m ()
writeBlock h hdr a (s,next) =
  dPut h a $ truncateString (blockSize hdr - 16) s `B.append` encode next

-- | Uses a given list of addresses to split a string into blocks and
-- write them to the addresses. 'OutOfSpace' will be raised if the
-- number of addresses in the list is not sufficient to store the
-- string.
writeBlocks :: Device m h
            => h
            -> Header
            -> [Address]
            -> ByteString
            -> m ()
writeBlocks h hdr adrs s
    | B.null s            = return ()
    | alen == blen        = f adrs (chopString (blockSize hdr - 16) s)
    | otherwise           = E.throw OutOfSpace
  where f (a:   _) (c:[]) = writeBlock h hdr a (c,0)
        f (a:n:as) (c:cs) = writeBlock h hdr a (c,n) >> f (n:as) cs
        blen              = B.length s `divBlocks` hdr
        alen              = genericLength $ genericTake blen adrs
                              -- not pretty, but it works with infinite lists.

truncateString l s = head (chopString l s)

chopString :: Integral l => l -> ByteString -> [ByteString]

chopString l s =
  case B.length s `compare` len of
    EQ -> [s]
    LT -> [s `B.append` B.replicate (len - B.length s) 0]
    GT -> let (a,b) = B.splitAt len s in a : chopString len b
  where len = fromIntegral l

divBlocks :: Integral i => i -> Header -> i

i `divBlocks` hdr
    | r == 0    = q
    | otherwise = q + 1
  where (q, r)  = i `quotRem` fromIntegral (blockSize hdr - 16)