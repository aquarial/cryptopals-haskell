{-# LANGUAGE OverloadedStrings #-}
module Challenge08 where

import           Challenge03                (englishScore)

import           Data.List
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as C8L


import Control.Lens ((^.))
import qualified Network.Wreq as Wreq


--c8 :: IO ()
c8 = do
  r <- Wreq.get "https://cryptopals.com/static/challenge-data/8.txt"
  let res = map (fst . B16.decode . C8L.toStrict) $ C8L.lines $ r ^. Wreq.responseBody
      decrypted = maximumBy (compare `on` numRepeats) $ res
  C8.putStrLn decrypted
  return ()

numRepeats :: ByteString -> Int
numRepeats b = length chnks - length (nub chnks)
  where
    chnks = chunks 16 b

chunks :: Int -> ByteString -> [ByteString]
chunks size bstr | B.length bstr == 0 = []
                 | otherwise          = B.take size bstr : chunks size (B.drop size bstr)

