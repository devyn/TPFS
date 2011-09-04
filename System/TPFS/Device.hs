{-# LANGUAGE MultiParamTypeClasses #-}

module System.TPFS.Device (Device(..)) where

import           Data.ByteString.Lazy (ByteString,hGet,hPut)
import           System.IO
import           System.TPFS.Address

class Monad m => Device m h where
  getDev :: h
         -> Address
         -> Int
         -> m ByteString

  putDev :: h
         -> Address
         -> ByteString
         -> m ()

instance Device IO Handle where
  getDev h off len =
    do hSeek h AbsoluteSeek (toInteger off)
       hGet  h len
  putDev h off s =
    do hSeek h AbsoluteSeek (toInteger off)
       hPut  h s