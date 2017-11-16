{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16

str :: ByteString
str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

convert :: ByteString -> ByteString
convert = B64.encode . fst . B16.decode

ans :: ByteString
ans = convert str
