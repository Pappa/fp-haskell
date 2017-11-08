module Guess (start) where

import Text.Read (readMaybe)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import Data.List (uncons)

numberRange = [1..10]

start :: IO ()
start =
  do putStrLn "Think of a number from 1 to 10."
     userNumber <- getLine
     let maybeInt = readMaybe userNumber :: Maybe Int
     case maybeInt of
         Just n  -> guessNumber n numberRange
         Nothing -> putStrLn "That's not a number, please try again."  >> start

guessNumber :: Int -> [Int] -> IO ()
guessNumber userNumber remaining = do
    putStrLn out
    if x > 0 then
        do
            answer <- getLine
            if answer == "y" then putStrLn "Yay!"
            else if answer == "n" then guessNumber userNumber xs
            else putStrLn "Please type y or n."  >> guessNumber userNumber remaining
    else
        return ()

    where 
        (x, xs) = getRandom remaining
        out
            | x == 0  = "Nothing left to guess. I think you cheated."
            | otherwise = "Is it " ++ (show x) ++ "? y/n"

getRandom :: [Int] -> (Int, [Int])
getRandom [] = (0, [])
getRandom xxs = 
    let result = uncons $ shuffled xxs
    in case result of
        Just (x, xs)  -> (x, xs)
        Nothing       -> (0, [])

shuffled :: [a] -> [a]
shuffled xs = shuffle' xs l gen
    where
        l = length xs
        gen = mkStdGen l

