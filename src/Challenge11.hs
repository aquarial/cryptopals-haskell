{-# LANGUAGE OverloadedStrings #-}
module Challenge11 where


import           Challenge07     (encryptECB)
import           Challenge10     (encryptCBC, decryptCBC)

import           Data.Word
import qualified Data.ByteString as B
import qualified System.Random   as R

key :: R.RandomGen t => t -> (B.ByteString, t)
key g = let (bytes,g2) = walk 16 [] g
        in (B.pack bytes, g2)
  where
    walk :: R.RandomGen t => Int -> [Word8] -> t -> ([Word8], t)
    walk 0 acc g = (acc,g)
    walk n acc g = let (r,g2) = R.random g
                   in walk (n-1) (r:acc) g2


randEncrypt = undefined
