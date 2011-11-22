-- | These are the objects of the TPFS system: namely, files and
-- tags. This module provides the low-level functions for managing
-- file/tag info and their respective tables, as well as somewhat
-- higher-level functions for more sane access.
module System.TPFS.Objects (
    -- * File objects
    -- ** The file table
    FileID,
    isFileIDInUse,
    lookupByFileID,
    addToFileTable,
    removeFromFileTable,
    -- ** File information blocks
    FileInfo(..),
    getFileInfo,
    putFileInfo,
    createFileInfo,
    removeFileInfo,
    -- ** High-level access
    -- * Tag objects
    -- ** The tag table
    TagHash,
    lookupByTagHash,
    addToTagTable,
    removeFromTagTable,
    -- ** Tag information blocks
    TagInfo(..),
    getTagInfo,
    putTagInfo,
    createTagInfo,
    removeTagInfo
    -- ** High-level access
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import           Data.Word
import           System.Random
import           System.TPFS.Address
import           System.TPFS.Device
import           System.TPFS.Filesystem

-- | File identification numbers are 128 bits long and are usually
-- randomly generated. As such, there is a 'Random' instance for
-- 'FileID's. Be careful when using this, however. It is possible
-- (though unlikely) for a collision to occur, so it is strongly
-- recommended to use 'isFileIDInUse' to ensure the 'FileID' is not
-- already in use when generating a 'FileID' in this manner.
data FileID = FileID !Word64 !Word64
            deriving Eq

instance Random FileID where
  randomR  = undefined
  random g = (FileID a b, g'') where (a, g' ) = random g
                                     (b, g'') = random g'

-- | Searches the file table to check if the given 'FileID' is
-- currently in use. This is useful when generating random 'FileID's.
isFileIDInUse :: Device m h
              => Filesystem m h
              -> FileID         -- ^ The 'FileID' to check.
              -> m Bool         -- ^ 'True' if the 'FileID' is
                                -- currently in use; 'False' if not.

isFileIDInUse = undefined

-- | Searches the file table for a given 'FileID' and returns the
-- address at which its 'FileInfo' can be found.
lookupByFileID :: Device m h
               => Filesystem m h
               -> FileID            -- ^ The ID of the file to look up.
               -> m (Maybe Address) -- ^ 'Nothing' if the 'FileID' does not exist;
                                    -- @'Just' 'Address'@ otherwise.

lookupByFileID = undefined

-- | Links a file information record to the file table so that it may
-- be discovered.
-- 
-- Note that a file information record (represented by 'FileInfo')
-- /must/ be linked to the file table in order for it to be found.
addToFileTable :: Device m h
               => Filesystem m h
               -> (FileID, Address) -- ^ The ID of the file in
                                    -- question, and the address of
                                    -- the first block of its record.
               -> m ()

addToFileTable = undefined

-- | Unlinks a file information table from the file table. This
-- prevents the file from being discovered.
-- 
-- It is possible for a file to be \'lost\' if it is not linked, so
-- take care when using this function manually.
removeFromFileTable :: Device m h
                    => Filesystem m h
                    -> FileID         -- ^ The file to be unlinked.
                    -> m ()

removeFromFileTable = undefined

-- | The 'FileInfo' structure attaches the blocks of a file to its
-- identification ('FileID' and tags) and other information vital for
-- reading the file (offset, length, lock).
data FileInfo = FileInfo { fileID     :: FileID    -- ^ The identification number of the file.
                         , firstBlock :: Address   -- ^ The first block in the file.
                         , lastBlock  :: Address   -- ^ The last block in the file. Makes appending faster.
                         , fileOffset :: Word64    -- ^ Describes the offset of the content within the blocks'
                                                   -- data. This could potentially allow for quick prepending of
                                                   -- data to the file.
                         , fileLength :: Word64    -- ^ The apparent (not necessarily actual block-wise) byte
                                                   -- length of the file.
                         , isLocked   :: Bool      -- ^ Whether the file is currently locked for writing. If the
                                                   -- file is locked for writing, the implementation must raise
                                                   -- an error when attempting to open the file for writing.
                         , tagHashes  :: [TagHash] -- ^ The hashes of each of the tags the file is attached to.
                         }

-- | Reads the file information from a file information record.
getFileInfo :: Device m h
            => Filesystem m h
            -> Address        -- ^ The first block of the file information record.
            -> m FileInfo

getFileInfo = undefined

-- | Modifies the file information in a file information record. Does
-- not affect tag structures linked via 'tagHashes'.
putFileInfo :: Device m h
            => Filesystem m h
            -> Address        -- ^ The first block of the file information record to be modified.
            -> FileInfo       -- ^ The file information to place in the record.
            -> m ()

putFileInfo = undefined

-- | Creates a new file information record. Won't link the file to the
-- file table, nor affect tag structures linked via 'tagHashes'.
createFileInfo :: Device m h
               => Filesystem m h
               -> FileInfo       -- ^ The initial contents of the file information record.
               -> m Address      -- ^ A pointer to the first block of the newly created record.

createFileInfo = undefined

-- | Frees the blocks used by a file information record. Does not free
-- blocks used by the file itself.
-- 
-- Note: 'removeFileInfo' won't automatically unlink the file from the
-- file table nor any tags linked to it. These operations should both
-- be done before calling 'removeFileInfo'.
removeFileInfo :: Device m h
               => Filesystem m h
               -> Address        -- ^ The address of the first block
                                 -- in the file information record to
                                 -- be removed.
               -> m ()

removeFileInfo = undefined

-- | Identifies tags with a SHA-256 hash of their data.
data TagHash = TagHash !Word64 !Word64 !Word64 !Word64
             deriving Eq

-- | Looks for a tag information record by its hash.
lookupByTagHash :: Device m h
                => Filesystem m h
                -> TagHash           -- ^ The hash of the tag to look up.
                -> m (Maybe Address) -- ^ 'Just' the address of the first block of the tag
                                     -- information record if found, or 'Nothing' if not found.

lookupByTagHash = undefined

-- | Links a tag information record to the tag table.
addToTagTable :: Device m h
              => Filesystem m h
              -> (TagHash, Address) -- ^ The hash of the tag in question, and the
                                    -- address of its information record.
              -> m ()

addToTagTable = undefined

-- | Removes a tag information record from the tag table.
-- 
-- Note: There is really no good reason to do this, as tags are
-- usually managed automatically.
removeFromTagTable :: Device m h
                   => Filesystem m h
                   -> TagHash
                   -> m ()

removeFromTagTable = undefined

-- | Describes a tag information record.
data TagInfo = TagInfo { tagHash :: TagHash       -- ^ The hash of the 'tagData'.
                       , tagData :: BS.ByteString -- ^ The data (contents) of the tag. The length/contents
                                                  -- of a tag's data are not restricted. Note: This is a strict
                                                  -- ByteString, unlike the lazy ByteStrings that are often
                                                  -- used throughout this package.
                       , fileIDs :: [FileID]      -- ^ The IDs of the files attached to the tag.
                       }

-- | Reads a 'TagInfo' structure from a tag information record on
-- disk.
getTagInfo :: Device m h
           => Filesystem m h
           -> Address        -- ^ The first block of the tag information record.
           -> m TagInfo

getTagInfo = undefined

-- | Writes a 'TagInfo' structure to a tag information record on disk.
putTagInfo :: Device m h
           => Filesystem m h
           -> Address        -- ^ The first block of the tag information record to be modified.
           -> TagInfo        -- ^ The tag information to place in the record.
           -> m ()

putTagInfo = undefined

-- | Creates a new tag information record. Won't link the tag to the
-- tag table, nor affect files linked via 'fileIDs'.
-- 
-- Note: A tag information record is fairly useless if not linked.
createTagInfo :: Device m h
              => Filesystem m h
              -> TagInfo        -- ^ The initial contents of the tag information record.
              -> m Address      -- ^ A pointer to the first block of the newly created record.

createTagInfo = undefined

-- | Frees the blocks used by a tag information record. This function
-- is mostly for internal use, as tag information records are usually
-- managed automatically.
-- 
-- Note: 'removeTagInfo' won't automatically unlink the tag from the
-- tag table nor any files linked to it. These operations should both
-- be done before calling 'removeTagInfo'.
removeTagInfo :: Device m h
              => Filesystem m h
              -> Address        -- ^ The address of the first block in
                                -- the tag information record to be
                                -- removed.
              -> m ()

removeTagInfo = undefined
