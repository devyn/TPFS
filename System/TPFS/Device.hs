{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines a readable and writable random-access device class, and some
-- supporting classes.
module System.TPFS.Device (Address, Device(..), Interleave(..), BinarySize(..)) where

import           Control.Applicative
import           Data.Binary
import           Data.ByteString.Lazy (ByteString,hGet,hPut)
import qualified Data.ByteString.Lazy as B
import           Data.Word
import           System.IO
import           System.IO.Unsafe (unsafeInterleaveIO)

type Address = Word64

class (Functor m, Applicative m, Monad m, Interleave m) => Device m h where
  -- | Reads bytes from the device. It should never return less than
  -- the requested number of bytes.
  dGet :: Integral i
       => h             -- ^ The device handle.
       -> Address       -- ^ The address to read from.
       -> i             -- ^ The number of bytes to read.
       -> m ByteString  -- ^ The read bytestring. If read past EOF,
                        -- the remaining bytes should be nulls.

  -- | Writes bytes to the device.
  dPut :: h           -- ^ The device handle.
       -> Address     -- ^ Address to write at.
       -> ByteString  -- ^ String to write.
       -> m ()

-- | Only necessary for IO -- the default implementation should be
-- sufficient for most monads.
class Monad m => Interleave m where
  interleave :: m a -> m a
  interleave  = id

instance Device IO Handle where
  dGet h off len =
    do hSeek h AbsoluteSeek (toInteger off)
       r <- hGetI h len
       if B.length r < fromIntegral len
          then return (r `B.append` B.replicate (fromIntegral len - B.length r) 0)
          else return  r
  dPut h off s =
    do hSeek h AbsoluteSeek (toInteger off)
       hPut  h s

hGetI          :: Integral l => Handle -> l -> IO ByteString
hGetI h l
    | l > mbi   = B.append <$> hGet h maxBound <*> hGetI h (l - mbi)
    | otherwise = hGet h (fromEnum l)
  where   mbi   = fromIntegral (maxBound :: Int)

instance Interleave IO where
  interleave = unsafeInterleaveIO

class Binary a => BinarySize a where
  -- | Returns a minimum and maximum size (in bytes) for an encoded object of
  -- this type. The actual value of the object itself should be discarded; this
  -- should be a constant.
  binarySize :: a -> (Integer, Integer)

instance BinarySize Word8 where binarySize = const (1,1)

instance BinarySize Word16 where binarySize = const (2,2)

instance BinarySize Word32 where binarySize = const (4,4)

instance BinarySize Word64 where binarySize = const (8,8)
