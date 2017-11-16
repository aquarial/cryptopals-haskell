{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString
import           Data.Word
import           Data.List.Split
import           Numeric
import           Data.Digits
import           Data.Char

a = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

num = fst $ head $ readHex a

f xs = intToChar $ fromIntegral $ (\x -> x `mod` 16) $ fst $ head $ readHex xs

d :: [[Char]]
d = chunksOf 4 a

dd = map (map intToChar . digits 64 . fst) $ concatMap readHex d

intToChar :: Int -> Char
intToChar x | 00 <= x && x <= 25 = chr (x - 00 + ord 'A')
            | 26 <= x && x <= 51 = chr (x - 26 + ord 'a')
            | 52 <= x && x <= 61 = chr (x - 52 + ord '0')
            | x == 62            = '+'
            | x == 63            = '/'
