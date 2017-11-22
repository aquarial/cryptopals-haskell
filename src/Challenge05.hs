{-# LANGUAGE OverloadedStrings #-}
module Challenge05 where

import           Challenge02            (xorB)

import           Data.Bits              (xor)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL

c5 :: ByteString
c5 = B16.encode $ repeatingXor "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"

repeatingXor :: ByteString -> BL.ByteString -> ByteString
repeatingXor a b = xorB a $ BL.toStrict $ BL.take len $ BL.cycle b
  where
    len = fromIntegral $ B.length a
