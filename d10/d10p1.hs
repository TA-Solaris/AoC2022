
import System.IO  
import Control.Monad

-- Defining Types
data Command = Noop | Addx Int
    deriving (Show, Read, Eq)

type His = [(Int, Int)]
type CPU = (Int, Int, His)

-- Getting Input
translateInput :: String -> [Command]
translateInput [] = []
translateInput str = [getCommand line | line <- lines str]
    where
        getCommand :: String -> Command
        getCommand "noop" = Noop
        getCommand ('a':'d':'d':'x':' ':x) = Addx (read x)
        getCommand _ = error "Invalid Command [Error 1]"

-- Defining helper functions
runCommand :: Command -> CPU -> CPU
runCommand Noop (x, c, his) = (x, c + 1, (x, c + 1):his)
runCommand (Addx v) (x, c, his) = (x + v, c + 2, (x, c + 2):(x, c + 1):his)

runCommands :: [Command] -> CPU -> CPU
runCommands cmds cpu = foldl (flip runCommand) cpu cmds

getHistory :: CPU -> His
getHistory (_, _, his) = reverse his

getScore :: His -> Int
getScore [] = 0
getScore ((x, c):his) | (c - 20) `mod` 40 == 0 = (x * c) + getScore his
                      | otherwise = getScore his

-- Part1
part1 :: [Command] -> Int
part1 cmds = getScore $ getHistory $ runCommands cmds (1, 0, [])

main = do
        handle <- openFile "d10/d10.txt" ReadMode
        contents <- hGetContents handle
        print $ part1 $ translateInput contents
        hClose handle
