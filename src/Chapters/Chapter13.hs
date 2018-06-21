module Chapters.Chapter13 where

import           Control.Monad
import           Data.Char     (isAlpha, toLower)
import           System.Exit   (exitSuccess)


palindrome :: IO ()
palindrome = forever $ do
    line1 <- wordsToStr <$> getLine
    if line1 == reverse line1 then
        putStrLn "It's a palindrome!"
    else putStrLn "Nope!" >> exitSuccess
    where wordsToStr = map toLower . filter isAlpha . concat . words


type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
      NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | age <= 0   = Left AgeTooLow
    | otherwise  =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    name <- putStrLn "Enter your name: " >> getLine
    age <- putStrLn "Enter your age: " >> read <$> getLine :: IO Integer
    case mkPerson name age of
        Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p
        Left e  -> print e
