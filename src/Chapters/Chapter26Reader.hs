module Chapters.Chapter26Reader where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy

rDec :: Num a => Reader a a
rDec = asks (+(-1))

rShow :: Show a => Reader a String
rShow = asks show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
    a <- ask
    liftIO (print ("Hi: " <> show a))
    return (a + 1)


sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
    a <- get
    liftIO (print ("Hi: " <> show a))
    put (a + 1)
    return $ show a


isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine
    guard $ isValid v
    return v


doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e  -> putStrLn ("Good, was very excite: " ++ e)
