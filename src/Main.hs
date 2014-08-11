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

twine :: [Match] -> [Match]
twine x = map (\(a,b) -> if odd a then b else flipM b) (zip [1 .. (length x)] x)

--main = do
--    putStrLn . show $ twine $ single [(Player "Sjur" 1200.0),
--        (Player "Nat" 1200.0),
--        (Player "Aasmund" 1200.0),
--        (Player "Trond" 1200.0)]

main = do
    putStrLn "Welcome to PubChess. Commands are help, single, double, list, result, save"
--    cmdLine <- getLine
    let cmd = words "help me please"
    process (head cmd) (tail cmd)

process :: String -> [String] -> IO ()
process "help" xs = help (head xs) (tail xs)
process _ xs = putStr "Unrecognized command"

help :: String -> [String] -> IO ()
help "help" _ = putStr "Prints some help, dude!"
help "list" _ = putStr "lists matches in tournament"
help "result" _ = putStr "result [match nr] <'white','black','remis'> adds a result"
help "save" _ = putStr "save tournament result to file"
help "single" _ = putStr "single Player1 Player2 .. Creates a tournament where all players play only 1 match against everyone else"
help "double" _ = putStr "double Player1 Player 2 .. Creates a tournament where all players play 2 matches against everyone else"

help x xs = putStr "Helping"
