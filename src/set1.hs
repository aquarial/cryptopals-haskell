{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import           Data.Bits (xor)
-- Challenge 1
c1 :: ByteString
c1 = hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: ByteString -> ByteString
hexToBase64 = B64.encode . fst . B16.decode


-- Challenge 2
c2 :: ByteString
c2 = xorByteString "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"

xorByteString :: ByteString -> ByteString -> ByteString
xorByteString a b = B16.encode $ B.pack $ B.zipWith xor (fst $ B16.decode a) (fst $ B16.decode b)



-- Challenge 3
