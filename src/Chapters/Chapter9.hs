module Chapters.Chapter9 where

import           Data.Char

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) | isUpper x = x:xs
                  | otherwise = toUpper x : xs

capitalize' :: String -> String
capitalize' [] = []
capitalize' (x:xs) | isUpper x = x: capitalize' xs
                   | otherwise = toUpper x : capitalize' xs


capHead :: String -> Char
capHead = toUpper . head
