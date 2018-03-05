{-# LANGUAGE OverloadedStrings #-}
module Challenge07 where

import           NetworkIO (getResource)
import           Challenge06                (decode64)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L

import           Crypto.Cipher.AES          (AES128)
import           Crypto.Cipher.Types        (BlockCipher (ecbEncrypt, ecbDecrypt),
                                             Cipher (cipherInit))
import           Crypto.Error               (throwCryptoError)

c7 :: IO ()
c7 = do
  txt <- getResource "https://cryptopals.com/static/challenge-data/7.txt"
  let msg = decode64 $ BL.toStrict $ C8L.filter (/= '\n') txt
  C8.putStrLn $ decryptECB "YELLOW SUBMARINE" msg

aeskey :: ByteString -> AES128
aeskey = throwCryptoError . cipherInit

encryptECB :: ByteString -> ByteString -> ByteString
encryptECB key bytes = ecbEncrypt (aeskey key) bytes

decryptECB :: ByteString -> ByteString -> ByteString
decryptECB key bytes = ecbDecrypt (aeskey key) bytes
