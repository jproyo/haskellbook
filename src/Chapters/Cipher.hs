module Chapters.Cipher (unCaesar, caesar) where

import           Data.Char (chr, ord)


caesar :: Int -> String -> String
caesar n = map (cypher n)

unCaesar :: Int -> String -> String
unCaesar n = map (unCypher n)

cypher :: Int -> Char -> Char
cypher n c = chr $ (ord c - 97 + n) `mod` 26 + 97

unCypher :: Int -> Char -> Char
unCypher n c = chr $ (ord c - 97 - n) `mod` 26 + 97
