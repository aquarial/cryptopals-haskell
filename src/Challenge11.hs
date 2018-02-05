{-# LANGUAGE OverloadedStrings #-}
module Challenge11 where


import           Challenge07          (decryptECB, encryptECB)
import           Challenge10          (decryptCBC, encryptCBC)

import qualified Control.Monad.Random as R
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import           Data.Monoid          ((<>))
import           Data.Word            (Word8)


rPad :: R.MonadRandom m => m ByteString
rPad = do len <- R.getRandomR (5, 10::Int)
          padding <- R.getRandomRs (minBound,maxBound::Word8)
          pure $ B.pack $ take len padding

padAround :: R.MonadRandom m => ByteString -> m ByteString
padAround text = do lpad <- randomByteString =<< R.getRandomR (5,10)
                    rpad <- randomByteString $ 16 - ((B.length lpad + B.length text) `mod` 16)
                    pure $ lpad <> text <> rpad


randomByteString :: R.MonadRandom m => Int -> m ByteString
randomByteString len = B.pack . take len <$> R.getRandomRs (minBound,maxBound)

randEncrypt :: R.MonadRandom m => ByteString -> m ByteString
randEncrypt itext = do text <- padAround itext
                       key <- randomByteString 16
                       mode <- R.getRandomR (False,True)
                       case mode of
                         False -> pure $ encryptECB key text
                         True  -> do iv <- randomByteString 16
                                     pure $ encryptCBC key iv text
