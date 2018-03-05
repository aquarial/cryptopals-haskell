{-# LANGUAGE OverloadedStrings #-}
module Challenge04 where


import           NetworkIO (getResource)
import           Challenge03                (englishScore, tryXorWithEachChar)

import           Data.Bits                  (xor)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Char                  (ord, toLower)
import           Data.Function              (on)
import           Data.List                  (maximumBy)

c4 :: IO ByteString
c4 = do txt <- getResource  "https://cryptopals.com/static/challenge-data/4.txt"
        pure $ process txt

process = mostEnglish . concatMap tryXorWithEachChar . toByteLines

toByteLines = map (fst . B16.decode . BL.toStrict) . C8L.lines
mostEnglish = maximumBy (compare `on` englishScore)

