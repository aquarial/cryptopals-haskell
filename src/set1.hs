{-# LANGUAGE OverloadedStrings #-}
import           Data.Bits (xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import qualified Data.Char              as C
import qualified Data.List              as L

-- Challenge 1
c1 :: ByteString
c1 = hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: ByteString -> ByteString
hexToBase64 = B64.encode . fst . B16.decode


-- Challenge 2
c2 :: ByteString
c2 = B16.encode $ xorB (b16to64 "1c0111001f010100061a024b53535009181c")
                       (b16to64 "686974207468652062756c6c277320657965")
  where
    b16to64 = fst . B16.decode

xorB :: ByteString -> ByteString -> ByteString
xorB a b = B.pack $ B.zipWith xor a b



-- Challenge 3
c3 = bestXor $ fst $ B16.decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"


bestXor :: ByteString -> ByteString
bestXor = C8.filter C.isPrint . last . L.sortOn xorScore . allXors

xorScore :: ByteString -> Int
xorScore = B.length . C8.filter C.isAlpha


allXors :: ByteString -> [ByteString]
allXors bs = [xorB b bs | b <- xorCodes (B.length bs)]

xorCodes :: Int -> [ByteString]
xorCodes len = map (C8.replicate len) $ concat [['0'..'9'], ['A'..'Z'], ['a'..'z']]
