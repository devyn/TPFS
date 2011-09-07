{-# LANGUAGE ViewPatterns #-}

module System.TPFS.Block where

import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           System.TPFS.Address
import           System.TPFS.Bitmap
import           System.TPFS.Device
import           System.TPFS.Header

-- | Reads a single block from the filesystem.
readBlock        :: Device m h
                 => h
                 -> Header
                 -> Address                        -- ^ The address of the block to read from.
                 -> m (ByteString, Maybe Address)  -- ^ The contents of the block, and a pointer to the
                                                   --   next block in the chain if there is one.
readBlock h hdr a = do c <- dGet h a (blockSize hdr)
                       let (bc, decode -> next)
                              = B.splitAt (fromIntegral $ blockSize hdr - 16) c
                           in return (bc, if next == 0
                                             then Nothing
                                             else Just next)

-- | Reads an entire block chain into a lazy bytestring.
readBlocks        :: Device m h => h -> Header -> Address -> m ByteString
readBlocks h hdr a = do (s,n) <- readBlock h hdr a
                        case n of
                          Nothing -> return s
                          Just a' -> (s `B.append`) `fmap` readBlocks h hdr a'