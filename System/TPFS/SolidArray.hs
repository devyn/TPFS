-- | Describes a class for accessing arrays on-disk, like bitmaps and
-- such.
module System.TPFS.SolidArray (SolidArray(..)) where

import System.TPFS.Address
import System.TPFS.Device

class SolidArray t where
  arrRead  :: (Device m h, Integral i) => h -> Address -> (i,i)    -> m [t]
  arrWrite :: (Device m h, Integral i) => h -> Address -> i -> [t] -> m ()