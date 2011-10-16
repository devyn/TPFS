-- | Describes a class for accessing arrays on-disk, like bitmaps and
-- such.
module System.TPFS.SolidArray (SolidArray(..)) where

import System.TPFS.Address
import System.TPFS.Device

-- | Solid arrays are on-disk arrays of values of type @t@. They can
-- be read from and written to with random access.
class SolidArray t where
  -- | Read an inclusive range from the array.
  arrRead  :: (Device m h, Integral i)
           => h        -- ^ The device handle.
           -> Address  -- ^ The base address of the array.
           -> (i,i)    -- ^ The range to read from the array.
           -> m [t]    -- ^ The resulting list of values.

  -- | Write a list to the array at an offset.
  arrWrite :: (Device m h, Integral i)
           => h        -- ^ The device handle.
           -> Address  -- ^ The base address of the array.
           -> i        -- ^ The offset to begin writing at.
           -> [t]      -- ^ The list of values to write at the offset.
           -> m ()