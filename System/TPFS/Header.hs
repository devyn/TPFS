module System.TPFS.Header (Header(..), getHeader, putHeader, headerSize) where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word
import           System.TPFS.Address
import           System.TPFS.Device

-- | The filesystem header.
data Header = Header { fileOffset  :: Address  -- ^ The address of the beginning of the file indexing table.
                     , maxFiles    :: Word64   -- ^ The maximum number of files supported.
                     , tagOffset   :: Address  -- ^ The address of the beginning of the tag indexing table.
                     , maxTags     :: Word64   -- ^ The maximum number of tags supported.
                     , blockBitmap :: Address  -- ^ The address of the beginning of the block allocation bitmap.
                     , blockOffset :: Address  -- ^ The beginning of the block space.
                     , blockSize   :: Word64   -- ^ The size of each block. Actual block content is
                                               -- @blockSize hdr - 16@ due to the pointer at the end of a block.
                     , maxBlocks   :: Word64   -- ^ Block capacity of the file system.
                     }
            deriving (Read, Show, Eq, Ord)

headerSize = 104

instance Binary Header where
  get     = do get :: Get Magic
               Header <$> get
                      <*> getWord64le
                      <*> get
                      <*> getWord64le
                      <*> get
                      <*> get
                      <*> getWord64le
                      <*> getWord64le

  put hdr = do put         $ Magic
               put         $ fileOffset  hdr
               putWord64le $ maxFiles    hdr
               put         $ tagOffset   hdr
               putWord64le $ maxFiles    hdr
               put         $ blockBitmap hdr
               put         $ blockOffset hdr
               putWord64le $ blockSize   hdr
               putWord64le $ maxBlocks   hdr

getHeader :: Device m h => h -> m Header
getHeader h = decode <$> dGet h 0 headerSize

putHeader :: Device m h => h -> Header -> m ()
putHeader h = dPut h 0 . encode

data Magic = Magic

instance Binary Magic where
  get       = do m <- mapM (const getWord8) [1..8]
                 case map (toEnum.fromEnum) m of
                   "__TPFS__" -> return Magic
                   _          -> fail "Invalid header."
  put Magic = mapM_ (putWord8.toEnum.fromEnum) "__TPFS__"