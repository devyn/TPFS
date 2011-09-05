module System.TPFS.Address (Address) where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Ratio
import           Data.Word
import           Text.Printf

data Address = Address Word64 Word64 deriving Eq

instance Ord Address where
  compare a@(Address al ah) b@(Address bl bh)
    | a  == b   = EQ
    | ah == bh  = compare al bl
    | otherwise = compare ah bh

instance Num Address where
  (Address al ah) + (Address bl bh) = Address cl ch
    where cl = al + bl
          ch = ah + bh + if cl < al then 1 else 0
  (Address al ah) - (Address bl bh) = Address cl ch
    where cl = al - bl
          ch = ah - bh - if al < bl then 1 else 0

  a * b                  = fromInteger (toInteger a * toInteger b)
  abs                    = id
  signum 0               = 0
  signum _               = 1
  fromInteger i          = uncurry Address . rtup . tup fromInteger $ i `divMod` (2^64)

instance Enum Address where
  toEnum   i             = uncurry Address . rtup . tup toEnum      $ i `divMod` (2^64)
  fromEnum (Address l h) = fromIntegral l + fromIntegral h * (2^64)
  succ     a             = a + 1
  pred     a             = a - 1

instance Real Address where
  toRational a = toInteger a % 1

instance Integral Address where
  toInteger (Address l h) = toInteger l + toInteger h * (2^64)
  quotRem   a1 a2         = tup fromInteger (toInteger a1 `quotRem` toInteger a2)

instance Bounded Address where
  maxBound = Address (negate 1) (negate 1)
  minBound = Address 0 0

instance Read Address where
  readsPrec d = map (\(a,b) -> (fromInteger a, b)) . readsPrec d

instance Show Address where
  showsPrec = const (showString . printf "0x%032x" . toInteger)

instance Binary Address where
  get      = Address <$> getWord64le
                     <*> getWord64le
  put (Address l h) = do putWord64le l
                         putWord64le h

tup f (a, b) = (f a, f b)

rtup  (a, b) = (b, a)