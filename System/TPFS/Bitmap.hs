module System.TPFS.Bitmap ( 
  -- * Reading
  bmpRead,
  bmpReadAt,
  -- * Writing
  bmpWriteRange,
  bmpSet,
  bmpClear,
  bmpSetAt,
  bmpClearAt,
  -- * Searching
  bmpAll,
  bmpFind
  ) where

import           Data.Bits
import qualified Data.ByteString.Lazy as B
import           Data.List
import           Data.Word
import           System.TPFS.Address
import           System.TPFS.Device

-- | Reads a range of bits from a bitmap as a list of booleans.
bmpRead :: (Device m h, Integral i) => h -> Address -> (i, i) -> m [Bool]
bmpRead h a (s,e) = (genericDrop s . genericTake (e+1) . B.foldr (\ w l -> bits w ++ l) [])
                    `fmap` dGet h base (byte2 - byte1 + 1)
  where (byte1, bit1) = s `divMod` 8
        (byte2, bit2) = e `divMod` 8
        base          = a + fromIntegral byte1
        (n,m)         = (bit2 + 1,bit1)

-- | Reads a single bit from a bitmap as a boolean.
bmpReadAt :: (Device m h, Integral i) => h -> Address -> i -> m Bool
bmpReadAt h a i = head `fmap` bmpRead h a (i,i)

-- | Given a state (False, True -> 0, 1), puts a range of a bitmap's bits in that state.
bmpWriteRange :: (Device m h, Integral i) => Bool -> h -> Address -> (i, i) -> m ()
bmpWriteRange t h a (s,e)
    | byte1 == byte2 = dGet h adr1 1 >>= dPut h adr1 . B.map (cbit [fromEnum bit1..fromEnum bit2])
    | otherwise      = do ch1 <- B.head `fmap` (dGet h adr1 1)
                          ch2 <- B.head `fmap` (dGet h adr2 1)
                          dPut h adr1 $ cbit [fromEnum bit1..7] ch1                      `B.cons`
                                        B.replicate (fromIntegral (byte2 - byte1) - 1) c `B.snoc`
                                        cbit [0..fromEnum bit2] ch2
  where (byte1, bit1) = s `divMod` 8
        (byte2, bit2) = e `divMod` 8
        ( adr1, adr2) = tup ((a +) . fromIntegral) (byte1, byte2)
        c             = if t then 0xFF else 0
        cbit seq b    = if t
                           then foldl (.|.) b $ map               bit  seq
                           else foldl (.&.) b $ map (complement . bit) seq

-- | Sets a range of bits in a bitmap.
bmpSet   :: (Device m h, Integral i) => h -> Address -> (i, i) -> m ()
bmpSet    = bmpWriteRange True

-- | Clears a range of bits in a bitmap.
bmpClear :: (Device m h, Integral i) => h -> Address -> (i, i) -> m ()
bmpClear  = bmpWriteRange False

-- | Sets a specific bit in a bitmap.
bmpSetAt        :: (Device m h, Integral i) => h -> Address -> i -> m ()
bmpSetAt   h a i = bmpWriteRange True  h a (i, i)

-- | Clears a specific bit in a bitmap.
bmpClearAt      :: (Device m h, Integral i) => h -> Address -> i -> m ()
bmpClearAt h a i = bmpWriteRange False h a (i, i)

-- | Returns a list of indices of all matched bits in a range of a
--   bitmap's bits.
bmpAll          :: (Device m h, Integral i)
                => h
                -> Address   -- ^ Base address of the bitmap.
                -> (i, i)    -- ^ Bit range to search in.
                -> Bool      -- ^ Bit to search for.
                -> m [i]
bmpAll h a r bit = (map snd . filter ((== bit).fst) . fold) `fmap` bmpRead h a r
  where fold     = snd . mapAccumL (\ i b -> (i+1, (b,i))) 0

-- | Searches for a specific bit in a bit range.
bmpFind          :: (Device m h, Integral i)
                 => h
                 -> Address      -- ^ Base address of the bitmap.
                 -> (i, i)       -- ^ Bit range to search in.
                 -> Bool         -- ^ Bit to search for.
                 -> m (Maybe i)  -- ^ Just the bit index of the first match, or Nothing if no match was found.
bmpFind h a r bit = do m <- bmpAll h a r bit
                       case m of
                         i:_ -> return (Just i)
                         []  -> return Nothing

bits n = [testBit n b | b <- [0 .. bitSize n - 1]]

tup f (a, b) = (f a, f b)