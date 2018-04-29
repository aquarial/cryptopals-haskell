{-# LANGUAGE OverloadedStrings #-}
module Challenge12 where

import           Challenge07            (encryptECB)
import           Challenge11            (detectEncrypt, EncryptionType(..))


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

prefixEncrypt :: R.MonadRandom m => B.ByteString -> m B.ByteString
prefixEncrypt itext = do
    let Right decoded = B64.decode secret
        txt           = itext <> decoded
        pad           = negate (B.length txt) `mod` 16
    pure $ encryptECB key (txt <> B.replicate pad 0)


detectBlockSize :: Monad m => (C8.ByteString -> m C8.ByteString) -> m Int
detectBlockSize encryptFunc = do
    res <- encryptFunc ""
    next <- runConduit
              $ yieldMany [0 ..]
              .| mapMC (\z -> encryptFunc (C8.replicate z '_'))
              .| mapC B.length
              .| filterC (/= B.length res)
              .| headC
    case next of
        Nothing -> error "Could not detect block size"
        Just n  -> pure $ n - B.length res

padToOneLessThan :: C8.ByteString -> Int -> C8.ByteString
padToOneLessThan b n =
    case (B.length b + 1) `mod` n of
      0 -> b
      _ -> padToOneLessThan (B.cons 0 b) n


--  |       |      |
--  asjfvpoasi
--  asjf
--
--  123456789123456789
--  0000000aabosidjvi
--  000000as
--  00000asj
--  0000asjf
--  asdjifaf



c12 :: IO ()
c12 = do
    blocksize <- detectBlockSize prefixEncrypt
    mode <- detectEncrypt prefixEncrypt
    msgLen <- B.length <$> prefixEncrypt ""
    case mode of
       CBCmode -> putStrLn "Can't break CBC like this yet"
       ECBmode -> loopBreak "" blocksize msgLen
  where
  findOneByte b block prefix = do blk <- prefixEncrypt (B.snoc prefix b)
                                  if block `B.isPrefixOf` blk
                                  then pure b
                                  else findOneByte (b+1) block prefix
  loopBreak bs size msgLen =
    if B.length bs >= msgLen
    then C8.putStrLn bs
    else do
      let padding = size - ((B.length bs + 1) `mod` size)
      matchbytes <- prefixEncrypt $ B.replicate padding 0
      let nearBlockStart = size * ((padding + B.length bs) `div` size)
          matchbyte = B.take size $ B.drop nearBlockStart matchbytes
          prefix = B.drop nearBlockStart (B.replicate padding 0 <> bs)
      byte <- findOneByte 0 matchbyte prefix
      loopBreak (B.snoc bs byte) size msgLen
