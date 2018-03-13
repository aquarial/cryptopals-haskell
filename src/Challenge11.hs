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
                       if mode 
                       then pure $ encryptECB key text
                       else do iv <- randomByteString 16
                               pure $ encryptCBC key iv text


chunksof :: Int -> ByteString -> [ByteString]
chunksof n xs = B.take n xs : chunksof n (B.drop n xs)

detectEncrypt :: (ByteString -> IO ByteString) -> IO Bool
detectEncrypt func = do output <- func $ B.replicate 1000 0
                        let inners = B.take 300 $ B.drop 100 output
                            parts = chunksof 16 inners
                        return $ and $ take 3 $ zipWith (==) parts (drop 1 parts)

c11 :: IO Bool
c11 = detectEncrypt randEncrypt
