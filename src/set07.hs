{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L

import           Control.Lens               ((^.))
import qualified Network.Wreq               as Wreq

import           Crypto.Cipher.AES          (AES128)
import           Crypto.Cipher.Types        (BlockCipher (ecbDecrypt),
                                             Cipher (cipherInit))
import           Crypto.Error               (throwCryptoError)

c7 :: IO ()
c7 = do
  r <- Wreq.get "https://cryptopals.com/static/challenge-data/7.txt"
  let msg = decode64 $ BL.toStrict $ C8L.filter (/= '\n') $ r ^. Wreq.responseBody
  C8.putStrLn $ decryptECB "YELLOW SUBMARINE" msg

decryptECB :: ByteString -> ByteString -> ByteString
decryptECB key bytes =  ecbDecrypt ctx bytes
  where
    ctx :: AES128
    ctx = throwCryptoError $ cipherInit key
