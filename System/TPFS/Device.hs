{-# LANGUAGE MultiParamTypeClasses #-}

module System.TPFS.Device (Device(..)) where

import           Control.Applicative
import           Data.ByteString.Lazy (ByteString,hGet,hPut)
import qualified Data.ByteString.Lazy as B
import           System.IO
import           System.TPFS.Address

class (Functor m, Applicative m, Monad m) => Device m h where
  dGet :: h
       -> Address
       -> Int
       -> m ByteString

  dPut :: h
       -> Address
       -> ByteString
       -> m ()

instance Device IO Handle where
  dGet h off len =
    do hSeek h AbsoluteSeek (toInteger off)
       r <- hGet h len
       if B.length r < toEnum len
          then return (r `B.append` B.replicate (toEnum len - B.length r) 0)
          else return  r
  dPut h off s =
    do hSeek h AbsoluteSeek (toInteger off)
       hPut  h s