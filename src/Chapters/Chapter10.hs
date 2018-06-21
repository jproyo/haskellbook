module Chapters.Chapter10 where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False


myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (x ==)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (x ==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr fltr []
    where fltr x a = if f x then x:a else a
