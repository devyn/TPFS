-- | Implements an instance for solid arrays of boolean values.
module System.TPFS.Bitmap (module System.TPFS.SolidArray) where

import           Control.Applicative
import           Data.Bits
import qualified Data.ByteString.Lazy as B
import           Data.List
import           System.TPFS.Device
import           System.TPFS.SolidArray

instance SolidArray Bool where
  -- This just simply converts the range into the byte range including
  -- that range, reads those bytes, converts those bytes into bits,
  -- and then crops the bits to the expected bit range.
  arrRead h a (s,e) = genericTake (e - s + 1)
                    . genericDrop sb
                    . concatMap bits
                    . B.unpack <$> dGet h (a + fromIntegral sB) (eB - sB + 1)
    where (sB, sb)  = s `divMod` 8
          (eB, eb)  = e `divMod` 8

  arrWrite h a o l
      = do -- We read the first and last byte in the range to change
           -- just in case the bits we want to change aren't right on
           -- the byte boundaries.
           bs1 <- bits . head . B.unpack <$> dGet h (a + fromIntegral sB) 1
           bs2 <- bits . head . B.unpack <$> dGet h (a + fromIntegral eB) 1
           -- We attempt to grab as many whole, aligned bytes as
           -- possible after the first byte.
           let (middle, rest) = bitsToString $ drop (8 - fromIntegral sb) l
           -- Now we write the byte string that we've compiled.
           dPut h (a + fromIntegral sB) $ B.concat [B.pack [fromBits $ genericTake sb bs1 ++ genericTake (8 - sb) l]
                                                   ,middle
                                                   ,if null rest
                                                       then B.empty
                                                       else B.pack [fromBits $ rest ++ genericDrop eb bs2]]
    where (sB, sb)  = o `divMod` 8
          (eB, eb)  = (o + genericLength l) `divMod` 8

-- | Turns a number into a list of booleans.
bits :: Bits a => a -> [Bool]

bits n = map (testBit n) [0..bitSize n - 1]

-- | Turns a list of booleans back into a number. Dual of 'bits'.
fromBits :: Bits a => [Bool] -> a

fromBits = fst . foldl (\(n,i) b -> ((if b then setBit else clearBit) n i, i + 1)) (0,0)

-- | Attempts to divide a list of bits into bytes and compile a
-- ByteString from them. Trailing bits that don't fit cleanly into
-- octets are returned alongside the result.
bitsToString :: [Bool] -> (B.ByteString, [Bool])

bitsToString  l = (B.pack (bytes l), drop (length l - length l `mod` 8) l)
  where bytes l | length l >= 8 = fromBits (take 8 l) : bytes (drop 8 l)
                | otherwise     =  []