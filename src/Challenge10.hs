{-# LANGUAGE OverloadedStrings #-}
module Challenge10 where


import           Challenge02                (xorB)
import           Challenge06                (decode64, chunks)
import           Challenge07                (decryptECB)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L

import           Control.Lens               ((^.))
import qualified Network.Wreq               as Wreq

import           Crypto.Cipher.AES          (AES128)
import           Crypto.Cipher.Types        (BlockCipher (ecbDecrypt),
                                             Cipher (cipherInit))
import           Crypto.Error               (throwCryptoError)

c10 = do
  r <- Wreq.get "https://cryptopals.com/static/challenge-data/10.txt"
  let body = decode64 $ C8L.toStrict $ C8L.filter (/= '\n') $ r ^. Wreq.responseBody
      parts = decryptCBC "YELLOW SUBMARINE" body
      text =  parts
  return text

decryptCBC :: ByteString -> ByteString -> ByteString
decryptCBC key body = C8.concat $ decryptCBCacc key (B.replicate 16 0) (chunks 16 body)

decryptCBCacc :: ByteString -> ByteString -> [ByteString] -> [ByteString]
decryptCBCacc key prev []     = []
decryptCBCacc key prev (m:ms) = xorB prev (decryptECB key m) : decryptCBCacc key m ms

