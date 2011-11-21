module System.TPFS.SuperBlock (
    module System.TPFS.SolidArray,
    SuperBlockState(..),
    bitsToSBStates,
    sbStatesToBits
  ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as B
import           System.TPFS.Address
import           System.TPFS.Bitmap
import           System.TPFS.SolidArray

data SuperBlockState = SBEmpty
                     | SBSpaceAvailable
                     | SBFull
                     deriving (Show, Read, Eq)

instance SolidArray SuperBlockState where
  arrRead  h a (s,e) = bitsToSBStates <$> arrRead h a (s*2,e*2+1)
  arrWrite h a o     = arrWrite h a (o*2) . sbStatesToBits

-- | Converts a list of bits into the equivalent @['SuperBlockState']@. The
-- input list length must be even, and the output list length will
-- always be half that of the input list.
bitsToSBStates (False : False : xs) = SBEmpty          : bitsToSBStates xs
bitsToSBStates (True  : False : xs) = SBSpaceAvailable : bitsToSBStates xs
bitsToSBStates (True  : True  : xs) = SBFull           : bitsToSBStates xs
bitsToSBStates []                   = []

-- | Converts @['SuperBlockState']@ into a list of bits. Dual of 'bitsToSBState'.
sbStatesToBits (SBEmpty          : xs) = False : False : sbStatesToBits xs
sbStatesToBits (SBSpaceAvailable : xs) = True  : False : sbStatesToBits xs
sbStatesToBits (SBFull           : xs) = True  : True  : sbStatesToBits xs
sbStatesToBits []                      = []