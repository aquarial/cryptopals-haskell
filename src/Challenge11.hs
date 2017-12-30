{-# LANGUAGE OverloadedStrings #-}
module Challenge11 where


import           Challenge07     (encryptECB, decryptECB)
import           Challenge10     (encryptCBC, decryptCBC)

import           Data.Word
import qualified Data.ByteString as B
import qualified System.Random   as R

sixteenrandoms :: R.RandomGen g => g -> B.ByteString
sixteenrandoms g = B.pack . take 16 . R.randoms $ g

randEncrypt g0 text = case choice::Int of
                        0 -> encryptECB cryptkey text
                        1 -> encryptCBC cryptkey iv text
  where
    (choice  , g1) = R.randomR (0,1) g0
    (g2      , g3) = R.split g1
    cryptkey       = sixteenrandoms g2
    iv             = sixteenrandoms g3

