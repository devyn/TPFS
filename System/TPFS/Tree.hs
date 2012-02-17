{-# LANGUAGE MultiParamTypeClasses #-}

-- | Various structures and algorithms for trees.
module System.TPFS.Tree
  ( BTree(..),
    BNode(..)
  ) where

import           System.TPFS.Device
import           System.TPFS.Filesystem

-- | Describes functions for accessing a B-tree on a 'Filesystem'. These
-- functions are used to write portable, reusable code for properly managing
-- any B-tree.
class Ord key => BTree key value pointer where

  -- | Returns the optimal Knuth order for the given filesystem.
  btOrder :: (Num a, Device m h)
          => Filesystem m h
          -> (key, value, pointer) -- ^ This parameter is included for the sole purpose of selecting which
                                   --   instance to use. Its value should be assumed undefined.
          -> a

  -- | Retrieves a 'BNode' from storage.
  --
  -- Might throw an exception (or use 'Maybe') in the future if the node failed
  -- verification (via a checksum or similar), but currently it does not.
  btGet :: Device m h
        => Filesystem m h
        -> pointer
        -> m (BNode key value pointer)

  -- | Places a 'BNode' into storage.
  -- 
  -- Attempting to write a 'BNode' to an unallocated part of storage may result
  -- in an exception in some implementations.
  btPut :: Device m h
        => Filesystem m h
        -> pointer
        -> BNode key value pointer
        -> m ()

  -- | Allocates and returns a pointer to a space where a 'BNode' may be stored.
  btAllocate :: Device m h
             => Filesystem m h
             -> m pointer

  -- | Frees the space used by a 'BNode'.
  btFree :: Device m h
         => Filesystem m h
         -> pointer
         -> m ()

-- | Describes a node of a B-tree in memory.
data BTree key value pointer => BNode key value pointer
     = BNode { bnKeys     :: [key]      -- ^ The keys used for branching in the tree. Must be comparable ('Ord').
             , bnValues   :: [value]    -- ^ The values associated with the keys. These values are not altered or
                                        --   inspected by any portable code.
             , bnChildren :: [pointer]  -- ^ The pointers for each of the branches of the node. Must be valid
                                        --   for use with the functions defined in the 'BTree' class.
             }


