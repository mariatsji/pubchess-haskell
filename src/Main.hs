module Main (
    main
) where

import Control.Monad
import Data.List
import Data.Bits
import Data.Maybe
import Data.Char
import System.Environment
import System.Directory
import System.IO

data Player = Player String deriving (Eq,Show)
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

players :: [String] -> [Player]
players [] = []
players x = map Player x

twine :: [Match] -> [Match]
twine x = map (\(a,b) -> if odd a then b else flipM b) (zip [1 .. (length x)] x)

result :: String -> [(Int, Match)] -> [(Int, Match)]
result cmd [] = []
result [] xs = []
result cmd xs = (take (n - 1) xs) ++ (n, matchResult (snd (xs !! (n - 1))) res) : (drop n xs)
                        where   n = read $ head $ words cmd
                                res = head $ tail $ words cmd

matchResult :: Match -> String -> Match
matchResult (Match white black result) "white" = Match white black White
matchResult (Match white black result) "black" = Match white black Black
matchResult (Match white black result) "remis" = Match white black Remis

createMatches :: String -> [Player] -> [Match]
createMatches [] [] = []
createMatches n [] = []
createMatches "single" p = twine $ single p
createMatches "double" p = double p

lines' :: [(Int, Match)] -> String
lines' m = unwords $ map (\m -> show m ++ "\n") m

main = do
    putStrLn "Welcome. Whats the tournament called?"
    tName <- getLine
    putStrLn "single or double?"
    single <- getLine
    putStrLn  "What are the players names (use space)?"
    playerN <- getLine
    let ps = players $ words playerN
        matches = createMatches single ps
        matches' = zip [1 .. (length matches)] matches
    process matches'
    putStrLn "Good games!"

process :: [(Int, Match)] -> IO ()
process m = do
    print' m
    putStrLn "Enter result (e.g. '1 white', '2 remis', '3 black')"
    cmd <- getLine
    let m' = result cmd m
    process m'

print' :: [(Int, Match)] -> IO ()
print' [] = putStrLn ""
print' m = mapM_ print m

save :: String -> [(Int, Match)] -> IO ()
save filename matches = do
    writeFile filename (lines' matches)
