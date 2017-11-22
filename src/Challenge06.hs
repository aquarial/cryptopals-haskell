{-# LANGUAGE OverloadedStrings #-}
module Challenge06 where

import           Challenge02                (xorB)
import           Challenge03                (englishScore)
import           Challenge05                (repeatingXor)

import qualified Data.Bits                  as Bits

import           Data.Function              (on)

import           Data.Either                (rights)
import qualified Data.List                  as L

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L

import           Control.Lens               ((^.))
import qualified Network.Wreq               as Wreq


c6 = do
  r <- Wreq.get "https://cryptopals.com/static/challenge-data/6.txt"
  let msg = decode64 $ BL.toStrict $ C8L.filter (/= '\n') $ r ^. Wreq.responseBody
      key = L.maximumBy (compare `on` (englishScore . repeatingXor msg . BL.fromStrict)) $ map (keyOfLen msg) [2..40]
  C8.putStrLn $ repeatingXor msg $ BL.fromStrict key
  return key

decode64 :: ByteString -> ByteString
decode64 = head . rights . flip (:) [] . B64.decode

keyOfLen :: ByteString -> Int -> ByteString
keyOfLen s k = B.concat $ map bestChar $ B.transpose $ chunks k s

chunks :: Int -> ByteString -> [ByteString]
chunks size bstr | B.length bstr == 0 = []
                 | otherwise          = B.take size bstr : chunks size (B.drop size bstr)

bestChar :: ByteString -> ByteString
bestChar bs = fst $ L.maximumBy (compare `on` (englishScore . snd)) $ oneCharWithXors bs

oneCharWithXors :: ByteString -> [(ByteString, ByteString)]
oneCharWithXors b = map (fmap (xorB b)) codes
  where
    codes :: [(ByteString, ByteString)]
    codes = map (\c -> (C8.singleton c, C8.replicate (B.length b) c)) chars
    chars :: [Char]
    chars = concat [['0'..'9'], ['A'..'Z'], ['a'..'z']]

