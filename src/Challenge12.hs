{-# LANGUAGE OverloadedStrings #-}
module Challenge12 where

import           Challenge07            (encryptECB, decryptECB)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Base64 as B64
import qualified Control.Monad.Random   as R
import           Data.Monoid            ((<>))
import           Conduit

key :: B.ByteString
key = "\215\166w\137\&0\231\139l\214Lp\STX\244\199\SI\145"

secret :: B.ByteString
secret = C8.concat
    [ "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg"
    , "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq"
    , "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg"
    , "YnkK"
    ]

randomByteString :: R.MonadRandom m => Int -> m B.ByteString
randomByteString len = B.pack . take len <$> R.getRandoms

randEncrypt :: R.MonadRandom m => B.ByteString -> m B.ByteString
randEncrypt itext = do
    let Right decoded = B64.decode secret
        txt           = itext <> decoded
        pad           = negate (B.length txt) `mod` 16
    pure $ encryptECB key (txt <> B.replicate pad 0)


--detectBlockSize :: (MonadCatch m, R.MonadIO m) => (C8.ByteString -> m a) -> m Int
detectBlockSize encryptFunc = do
    res <- encryptFunc ""
    next <-
        runConduit
        $ yieldMany [0 ..]
        .| mapMC (\z -> encryptFunc (C8.replicate z '_'))
        .| mapC B.length
        .| filterC (/= B.length res)
        .| headC
    case next of
        Nothing -> error "Could not detect block size"
        Just n  -> pure $ n - B.length res

c12 :: IO Int
c12 = detectBlockSize randEncrypt
