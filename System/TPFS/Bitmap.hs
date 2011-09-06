module System.TPFS.Bitmap ( 
  -- * Reading
  bmpRead,
  bmpReadAt,
  -- * Writing
  bmpWriteRange,
  bmpSet,
  bmpClear,
  bmpSetAt,
  bmpClearAt
  ) where

import           Data.Bits
import qualified Data.ByteString.Lazy as B
import           Data.Word
import           System.TPFS.Address
import           System.TPFS.Device

-- | Reads a range of bits from a bitmap as a list of booleans.
bmpRead :: Device m h => h -> Address -> (Int, Int) -> m [Bool]
bmpRead h a (s,e) = (drop s . take (e+1) . B.foldr (\ w l -> bits w ++ l) [])
                    `fmap` dGet h base (byte2 - byte1 + 1)
  where (byte1, bit1) = s `divMod` 8
        (byte2, bit2) = e `divMod` 8
        base          = a + toEnum byte1
        (n,m)         = (bit2 + 1,bit1)

-- | Reads a single bit from a bitmap as a boolean.
bmpReadAt :: Device m h => h -> Address -> Int -> m Bool
bmpReadAt h a i = head `fmap` bmpRead h a (i,i)

-- | Given a state (False, True -> 0, 1), puts a range of a bitmap's bits in that state.
bmpWriteRange :: Device m h => Bool -> h -> Address -> (Int, Int) -> m ()
bmpWriteRange t h a (s,e)
    | byte1 == byte2 = dGet h adr1 1 >>= dPut h adr1 . B.map (cbit [bit1..bit2])
    | otherwise      = do ch1 <- B.head `fmap` (dGet h adr1 1)
                          ch2 <- B.head `fmap` (dGet h adr2 1)
                          dPut h adr1 $ cbit [bit1..7] ch1                               `B.cons`
                                        B.replicate (fromIntegral (byte2 - byte1) - 1) c `B.snoc`
                                        cbit [0..bit2] ch2
  where (byte1, bit1) = s `divMod` 8
        (byte2, bit2) = e `divMod` 8
        ( adr1, adr2) = tup ((a +) . toEnum) (byte1, byte2)
        c             = if t then 0xFF else 0
        cbit seq      = if t
                           then flip (foldl (.|.)) $ map               bit  seq
                           else flip (foldl (.&.)) $ map (complement . bit) seq

-- | Sets a range of bits in a bitmap.
bmpSet   :: Device m h => h -> Address -> (Int, Int) -> m ()
bmpSet    = bmpWriteRange True

-- | Clears a range of bits in a bitmap.
bmpClear :: Device m h => h -> Address -> (Int, Int) -> m ()
bmpClear  = bmpWriteRange False

-- | Sets a specific bit in a bitmap.
bmpSetAt        :: Device m h => h -> Address -> Int -> m ()
bmpSetAt   h a i = bmpWriteRange True  h a (i, i)

-- | Clears a specific bit in a bitmap.
bmpClearAt      :: Device m h => h -> Address -> Int -> m ()
bmpClearAt h a i = bmpWriteRange False h a (i, i)

bits n = [testBit n b | b <- [0 .. bitSize n - 1]]

tup f (a, b) = (f a, f b)