{-# LANGUAGE OverloadedStrings #-}
module Challenge04 where

import           Challenge03                      (englishScore, tryXorWithEachChar)

import           Control.Lens               ((^.))
import           Data.Bits                  (xor)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Char                  (ord)
import           Data.Char                  (toLower)
import           Data.Function              (on)
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (maximumBy)
import qualified Network.Wreq               as Wreq

c4 :: IO ByteString
c4 = do
  a <- Wreq.get "https://cryptopals.com/static/challenge-data/4.txt"
  let body = a ^. Wreq.responseBody
      strs = map (fst . B16.decode . BL.toStrict) $ C8L.lines body
      possibleOutcomes = concatMap tryXorWithEachChar strs
      mostEnglish = maximumBy (compare `on` englishScore) possibleOutcomes
  return mostEnglish
