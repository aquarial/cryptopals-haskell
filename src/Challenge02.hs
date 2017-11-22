{-# LANGUAGE OverloadedStrings #-}
module Challenge02 where

import qualified Data.Bits              as Bits

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16

c2 :: ByteString
c2 = B16.encode $ xorB (b16decode "1c0111001f010100061a024b53535009181c")
                       (b16decode "686974207468652062756c6c277320657965")
  where
    b16decode = fst . B16.decode

xorB :: ByteString -> ByteString -> ByteString
xorB a b = B.pack $ B.zipWith Bits.xor a b

