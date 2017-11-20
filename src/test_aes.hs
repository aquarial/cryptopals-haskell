{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import           Crypto.Error (CryptoFailable(..), CryptoError(..))

import qualified Crypto.Random.Types as CRT

import           Data.ByteArray (ByteArray)
import           Data.ByteString (ByteString)

-- | Not required, but most general implementation
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genSecretKey :: forall m c a. (CRT.MonadRandom m, BlockCipher c, ByteArray a) => c -> Int -> m (Key c a)
genSecretKey _ = fmap Key . CRT.getRandomBytes

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (CRT.MonadRandom m, BlockCipher c) => c -> m (Maybe (IV c))
genRandomIV _ = do
  bytes :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: c)
  return $ makeIV bytes

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ctrCombine c initIV msg

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decrypt = encrypt

exampleAES256 :: ByteString -> IO ()
exampleAES256 msg = do
  -- secret key needs 256 bits (32 * 8)
  secretKey <- genSecretKey (undefined :: AES256) 32
  mInitIV <- genRandomIV (undefined :: AES256)
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let encryptedMsg = encrypt secretKey initIV msg
          decryptedMsg = decrypt secretKey initIV =<< encryptedMsg
      case (,) <$> encryptedMsg <*> decryptedMsg of
        Left err -> error $ show err
        Right (eMsg, dMsg) -> do
          putStrLn $ "Original Message: " ++ show msg
          putStrLn $ "Message after encryption: " ++ show eMsg
          putStrLn $ "Message after decryption: " ++ show dMsg
