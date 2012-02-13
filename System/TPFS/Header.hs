module System.TPFS.Header (Header(..), getHeader, putHeader, headerSize) where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           System.TPFS.Device

-- | A TPFS filesystem header, which is located at offset 0 in any
-- filesystem, and is of length 'headerSize' bytes.
data Header = Header { fileOffset    :: Address  -- ^ The address of the beginning of the file indexing table.
                     , maxFiles      :: Word64   -- ^ The maximum number of files supported.
                     , tagOffset     :: Address  -- ^ The address of the beginning of the tag indexing table.
                     , maxTags       :: Word64   -- ^ The maximum number of tags supported.
                     , superBlockMap :: Address  -- ^ The address to the superblock-level bitmap.
                     , blockMap      :: Address  -- ^ The address to the block-level bitmap.
                     , blockOffset   :: Address  -- ^ The beginning of the block space.
                     , blockSize     :: Word64   -- ^ The size of each block. Actual block content is
                                                 -- @blockSize hdr - 16@ due to the pointer at the end of a block.
                     , maxBlocks     :: Word64   -- ^ Block capacity of the file system.
                     }
            deriving (Read, Show, Eq, Ord)

-- | The calculated size of a header in bytes. It is constant for any
-- header.
headerSize = 80

instance Binary Header where
  get     = do get :: Get Magic
               Header <$> getWord64le
                      <*> getWord64le
                      <*> getWord64le
                      <*> getWord64le
                      <*> getWord64le
                      <*> getWord64le
                      <*> getWord64le
                      <*> getWord64le
                      <*> getWord64le

  put hdr = do put         $ Magic
               putWord64le $ fileOffset    hdr
               putWord64le $ maxFiles      hdr
               putWord64le $ tagOffset     hdr
               putWord64le $ maxFiles      hdr
               putWord64le $ superBlockMap hdr
               putWord64le $ blockMap      hdr
               putWord64le $ blockOffset   hdr
               putWord64le $ blockSize     hdr
               putWord64le $ maxBlocks     hdr

-- | Loads the filesystem header from a device.
getHeader :: Device m h => h -> m Header
getHeader h = decode <$> dGet h 0 headerSize

-- | Writes a filesystem header to the device.
putHeader :: Device m h => h -> Header -> m ()
putHeader h = dPut h 0 . encode

data Magic = Magic

instance Binary Magic where
  get       = do m <- mapM (const getWord8) [1..8]
                 case map (toEnum.fromEnum) m of
                   "TPFS0000" -> return Magic
                   _          -> fail "Invalid header."
  put Magic = mapM_ (putWord8.toEnum.fromEnum) "TPFS0000"
