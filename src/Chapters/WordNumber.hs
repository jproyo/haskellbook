module Chapters.WordNumber where

import           Data.List (intercalate)

digitWords :: [String]
digitWords = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitToWord :: Int -> String
digitToWord n = digitWords !! n

digits :: Int -> [Int]
digits = generate []
    where generate acc 0 = acc
          generate acc x = generate (mod x 10:acc) (div x 10)

wordNumber :: Int -> String
wordNumber n = intercalate "-" . map digitToWord $ digits n
