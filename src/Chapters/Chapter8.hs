module Chapters.Chapter8 where

import           Data.List (intercalate)

sumNum :: (Eq a, Num a) => a -> a
sumNum = sumTail 0
    where sumTail acc 0 = acc
          sumTail acc n = sumTail (acc+n) (n-1)

multInt :: (Integral a) => a -> a -> a
multInt _ 0 = 0
multInt 0 _ = 0
multInt n m = multTail 0 n m
    where multTail acc _ 0 = acc
          multTail acc x y = multTail (acc+x) x (y-1)


mc91 :: (Integral a) => a -> a
mc91 n
    | n > 100   = n - 10
    | otherwise = mc91 (mc91 (n+11))

digitWords :: [String]
digitWords = ["cero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitToWord :: Int -> String
digitToWord n = digitWords !! n

digits :: Int -> [Int]
digits = generate []
    where generate acc 0 = acc
          generate acc x = generate (mod x 10:acc) (div x 10)

wordNumber :: Int -> String
wordNumber n = intercalate "-" $ map digitToWord (digits n)
