{-# LANGUAGE ExistentialQuantification #-}

-- | Functions for creation and initialization of filesystems.
module System.TPFS.Filesystem (
    Filesystem(..),
    fsFromFile,
    newFSInFile
  ) where

import System.TPFS.Device
import System.TPFS.Header
import System.IO

-- | Contains all of the information necessary to access the
-- filesystem.
data Device m h => Filesystem m h = Filesystem { fsHandle :: h
                                               , fsHeader :: Header }
                                  deriving Show

-- | Opens a filesystem stored in a file.
fsFromFile :: String                    -- ^ The path to the file containing the filesystem.
           -> IO (Filesystem IO Handle) -- ^ The opened filesystem.

fsFromFile path = do h   <- openFile path ReadWriteMode
                     hdr <- getHeader h
                     return $ Filesystem h hdr

-- | Creates a new filesystem within a file.
newFSInFile :: Header                    -- ^ The filesystem header.
            -> String                    -- ^ The path to the file that should contain the filesystem.
            -> IO (Filesystem IO Handle) -- ^ The created filesystem.

newFSInFile hdr path = do h <- openFile path ReadWriteMode
                          putHeader h hdr
                          return $ Filesystem h hdr