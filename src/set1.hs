{-# LANGUAGE OverloadedStrings #-}
import Data.Word
import Data.Char
import Data.Bits
import Data.List
import Numeric
import Data.List.Split (chunksOf)
import Data.Digits

d :: [[Char]]
d = chunksOf 4 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

dd = map (map intToChar . digits 64 . fst) $ concatMap readHex d

intToChar :: Int -> Char
intToChar x | 00 <= x && x <= 25 = chr (x - 00 + ord 'A')
            | 26 <= x && x <= 51 = chr (x - 26 + ord 'a')
            | 52 <= x && x <= 61 = chr (x - 52 + ord '0')
            | x == 62            = '+'
            | x == 63            = '/'
