module Chapters.Chapter7 where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigitDivMod :: Integral a => a -> a
tensDigitDivMod x = fst $ divMod x 10


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show
