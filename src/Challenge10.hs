{-# LANGUAGE OverloadedStrings #-}
module Challenge10 where


import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L

import           Control.Lens               ((^.))
import qualified Network.Wreq               as Wreq

import           Crypto.Cipher.AES          (AES128)
import           Crypto.Cipher.Types        (BlockCipher (ecbDecrypt),
                                             Cipher (cipherInit))
import           Crypto.Error               (throwCryptoError)

decryptECB :: ByteString -> ByteString -> ByteString
decryptECB key bytes = ecbDecrypt ctx bytes
  where
    ctx :: AES128
    ctx = throwCryptoError $ cipherInit key
