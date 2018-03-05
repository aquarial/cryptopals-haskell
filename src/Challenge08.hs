{-# LANGUAGE OverloadedStrings #-}
module Challenge08 where

import           NetworkIO (getResource)
import           Challenge03                (englishScore, tryXorWithEachChar)
import           Challenge06                (chunks)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Function              (on)
import           Data.List

c8 :: IO ()
c8 = do
  txt <- getResource "https://cryptopals.com/static/challenge-data/8.txt"
  let res = map (fst . B16.decode . C8L.toStrict) $ C8L.lines txt
      decrypted = maximumBy (compare `on` numRepeats) res
  C8.putStrLn decrypted
  return ()

numRepeats :: ByteString -> Int
numRepeats b = length chnks - length (nub chnks)
  where
    chnks = chunks 16 b
