{-# LANGUAGE ScopedTypeVariables #-}

module Chapters.Chapter14Spec (spec) where

import           Data.List             (sort)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, _)       = (Just y, x >= y)

nonZero :: Gen Integer
nonZero = fmap getNonZero arbitrary

prop_nonZero_Quot_Rem :: Property
prop_nonZero_Quot_Rem = forAll nonZero $ \a -> forAll nonZero $ \b -> quot a b * b + rem a b == a

prop_nonZero_Div_Mod :: Property
prop_nonZero_Div_Mod = forAll nonZero $ \a -> forAll nonZero $ \b -> div a b * b + mod a b == a

spec :: Spec
spec = do
    describe "half properties" $
        modifyMaxSize (const 1000) $ prop "halfIdentity == (*2) . half" $
            \(x :: Rational) -> 2 * half x == x
    describe "list sort" $
        modifyMaxSize (const 1000) $ prop "True == listOrdered . sort" $
            \(xs :: [Integer]) -> listOrdered $ sort xs
    describe "associative and commutative on Sum" $ do
        modifyMaxSize (const 1000) $ prop "is Associative x + (y + z) == (x + y) + z" $
            \(x::Integer) (y::Integer) (z::Integer) -> (x + y) + z == x + (y + z)
        modifyMaxSize (const 1000) $ prop "is Commutative x + y == y + x" $
            \(x::Integer) (y::Integer) -> x + y == y + x
    describe "associative and commutative on Mult" $ do
        modifyMaxSize (const 1000) $ prop "is Associative x * (y * z) == (x * y) * z" $
            \(x::Integer) (y::Integer) (z::Integer) -> (x * y) * z == x * (y * z)
        modifyMaxSize (const 1000) $ prop "is Commutative x * y == y * x" $
            \(x::Integer) (y::Integer) -> x * y == y * x
    describe "quot rem div relationship" $ do
        modifyMaxSize (const 1000) $ prop "quot and rem relationship"
            prop_nonZero_Quot_Rem
        modifyMaxSize (const 1000) $ prop "div and mod relationship"
            prop_nonZero_Div_Mod
    describe "reverse list" $
        modifyMaxSize (const 1000) $ prop "twice == id" $
            \(xs :: [Int]) -> (reverse . reverse) xs == xs
    describe "Function composition" $ do
        modifyMaxSize (const 1000) $ prop "f $ a == f a" $
            \(x :: Int) -> id $ x == id x
        modifyMaxSize (const 1000) $ prop "f . g == f (g x)" $
            \(x :: Int) -> ((+1) . (*2)) x == (x * 2) + 1
    describe "foldr" $ do
        prop "foldr (:) == (++)" $
            \(xs :: [Int]) -> foldr (:) [] xs == [] ++ xs
        prop "foldr (++) [] == concat" $
            \(xs :: [[Int]]) -> foldr (++) [] xs == concat xs
    describe "read show roundtrip" $
        modifyMaxSize (const 1000) $ prop "(read (show x)) == x" $
            \(x :: String) -> (read (show x)) == x
