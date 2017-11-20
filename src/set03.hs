{-# LANGUAGE OverloadedStrings #-}
module C3 where

import           Data.Bits              (xor)
import           Data.Char              (ord, toLower)
import           Data.Function          (on)

import           Data.List              (maximumBy)

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8

import qualified Data.HashMap.Strict    as Map

c3 :: ByteString
c3 = decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  where
    decode :: ByteString -> ByteString
    decode = maximumBy (compare `on` englishScore) . tryXorWithEachChar . fst . B16.decode

englishScore :: ByteString -> Double
englishScore =  sum . map (\c -> Map.lookupDefault 0 c freq) . map toLower . C8.unpack
  where
    freq = Map.fromList [('a', 8.167), ('b', 1.492), ('c', 2.782), ('d', 4.253), ('e', 12.70), ('f', 2.228),
                         ('g', 2.015), ('h', 6.094), ('i', 6.966), ('j', 0.153), ('k', 0.772), ('l', 4.025),
                         ('m', 2.406), ('n', 6.749), ('o', 7.507), ('p', 1.929), ('q', 0.095), ('r', 5.987),
                         ('s', 6.327), ('t', 9.056), ('u', 2.758), ('v', 0.978), ('w', 2.360), ('x', 0.150),
                         ('y', 1.974), ('z', 0.074), (' ', 18.10)]

tryXorWithEachChar :: ByteString -> [ByteString]
tryXorWithEachChar bs = [xorB b bs | b <- xorCodes (B.length bs)]
  where
    xorCodes :: Int -> [ByteString]
    xorCodes len = map (C8.replicate len) $ concat [['0'..'9'], ['A'..'Z'], ['a'..'z']]

xorB :: ByteString -> ByteString -> ByteString
xorB a b = B.pack $ B.zipWith xor a b
