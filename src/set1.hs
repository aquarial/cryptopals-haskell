{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Bits as Bits

import           Data.Function (on)

import           Data.Char (ord)
import qualified Data.Char              as C

import qualified Data.List              as L

import           Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy   as BL

import           Control.Lens ((^.))
import qualified Network.Wreq           as Wreq

import qualified Data.HashMap.Strict    as Map



-- Challenge 1
c1 :: ByteString
c1 = hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: ByteString -> ByteString
hexToBase64 = B64.encode . fst . B16.decode



-- Challenge 2
c2 :: ByteString
c2 = B16.encode $ xorB (b16decode "1c0111001f010100061a024b53535009181c")
                       (b16decode "686974207468652062756c6c277320657965")
  where
    b16decode = fst . B16.decode

xorB :: ByteString -> ByteString -> ByteString
xorB a b = B.pack $ B.zipWith Bits.xor a b



-- Challenge 3
c3 :: ByteString
c3 = decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  where
    decode :: ByteString -> ByteString
    decode = L.maximumBy (compare `on` englishScore) . allOneCharXors . fst . B16.decode

englishScore :: ByteString -> Double
englishScore =  sum . map (\c -> Map.lookupDefault 0 c freq) . map C.toLower . C8.unpack
  where
    freq = Map.fromList [('a', 8.167), ('b', 1.492), ('c', 2.782), ('d', 4.253), ('e', 12.70), ('f', 2.228),
                         ('g', 2.015), ('h', 6.094), ('i', 6.966), ('j', 0.153), ('k', 0.772), ('l', 4.025),
                         ('m', 2.406), ('n', 6.749), ('o', 7.507), ('p', 1.929), ('q', 0.095), ('r', 5.987),
                         ('s', 6.327), ('t', 9.056), ('u', 2.758), ('v', 0.978), ('w', 2.360), ('x', 0.150),
                         ('y', 1.974), ('z', 0.074), (' ', 18.10)]

allOneCharXors :: ByteString -> [ByteString]
allOneCharXors bs = [xorB b bs | b <- xorCodes (B.length bs)]
  where
    xorCodes :: Int -> [ByteString]
    xorCodes len = map (C8.replicate len) $ concat [['0'..'9'], ['A'..'Z'], ['a'..'z']]




-- Challenge 4
c4 :: IO ByteString
c4 = do
  a <- Wreq.get "https://cryptopals.com/static/challenge-data/4.txt"
  let body = a ^. Wreq.responseBody
      strs = map (fst . B16.decode . BL.toStrict) $ BL.split nline body
      possibleOutcomes = concatMap allOneCharXors strs
      mostEnglish = L.maximumBy (compare `on` englishScore) possibleOutcomes
  return mostEnglish
  where
    nline = fromIntegral (ord '\n')


-- Challenge 5
c5 = B16.encode $ repeatingXor "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"

repeatingXor :: ByteString -> BL.ByteString -> ByteString
repeatingXor a b = xorB a $ BL.toStrict $ BL.take len (BL.cycle b)
  where
    len = fromIntegral $ B.length a



-- Challenge 6
c6 = undefined

rateKeySize :: ByteString -> Int -> Double
rateKeySize str keysize = ((/) `on` fromIntegral) editDistance keysize
  where
    editDistance = sum $ take (4 * keysize) $ B.zipWith ((+) `on` Bits.popCount) str (B.drop keysize str)

hammingDist :: ByteString -> ByteString -> Int
hammingDist a b = B.foldl' (\a b -> a + Bits.popCount b) 0 $ xorB a b

