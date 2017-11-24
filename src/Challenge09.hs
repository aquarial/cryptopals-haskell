{-# LANGUAGE OverloadedStrings #-}
module Challenge09 where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

c9 :: ByteString
c9 = padTo 20 "YELLOW SUBMARINE"

padTo :: Int -> ByteString -> ByteString
padTo size b = B.concat [b, B.replicate (size - B.length b) 4]
