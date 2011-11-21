{-# LANGUAGE ExistentialQuantification #-}

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

fsFromFile :: String -> IO (Filesystem IO Handle)

fsFromFile path = do h   <- openFile path ReadWriteMode
                     hdr <- getHeader h
                     return $ Filesystem h hdr

newFSInFile :: Header -> String -> IO (Filesystem IO Handle)

newFSInFile hdr path = do h <- openFile path ReadWriteMode
                          putHeader h hdr
                          return $ Filesystem h hdr