module Main (
    main
) where

import Data.List
import Data.Bits
import Data.Maybe
import System.Environment
import System.Directory
import System.IO

data Player = Player String Float deriving (Eq,Show)
data Result = Unplayed | Remis | White | Black deriving (Eq,Show)
data Match = Match Player Player Result deriving (Eq,Show)

flipM :: Match -> Match
flipM (Match white black result) = (Match black white result)

combos :: Player -> [Player] -> [Match]
combos _ [] = []
combos x (xs) = (Match x (head xs) Unplayed) : combos x (tail xs)

single :: [Player] -> [Match]
single [] = []
single (x:xs) = (combos x xs) ++ if (null xs)
                                    then []
                                    else single xs

double :: [Player] -> [Match]
double x = single x ++ map flipM (single x)

twine :: [Match] -> [Match]
twine x = map (\(a,b) -> if odd a then b else flipM b) (zip [1 .. (length x)] x)

main = do
    args <- getArgs
    cmd <- getLine
    process $ words cmd

process :: [String] -> IO ()
process [] = "Unknown command"
process _ = putStrLn "Processing .."
