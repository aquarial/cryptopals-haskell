{-# LANGUAGE OverloadedStrings #-}
module Challenge10 where


import           Challenge02                (xorB)
import           Challenge06                (decode64, chunks)
import           Challenge07                (encryptECB, decryptECB)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L

import           Control.Lens               ((^.))
import qualified Network.Wreq               as Wreq

c10 = do
  r <- Wreq.get "https://cryptopals.com/static/challenge-data/10.txt"
  let body = decode64 $ C8L.toStrict $ C8L.filter (/= '\n') $ r ^. Wreq.responseBody
      text = decryptCBC "YELLOW SUBMARINE" zeroIV body
  return text

-- https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#CBC

zeroIV = B.replicate 16 0

encryptCBC :: ByteString -> ByteString -> ByteString -> ByteString
encryptCBC key iv body = C8.concat $ encryptCBCacc key iv (chunks 16 body)

encryptCBCacc :: ByteString -> ByteString -> [ByteString] -> [ByteString]
encryptCBCacc key prev []     = []
encryptCBCacc key prev (p:ps) = curr : encryptCBCacc key curr ps
  where
    curr = encryptECB key (xorB prev p)


decryptCBC :: ByteString -> ByteString -> ByteString -> ByteString
decryptCBC key iv body = C8.concat $ decryptCBCacc key iv (chunks 16 body)

decryptCBCacc :: ByteString -> ByteString -> [ByteString] -> [ByteString]
decryptCBCacc key prev []     = []
decryptCBCacc key prev (c:cs) = xorB prev (decryptECB key c) : decryptCBCacc key c cs
