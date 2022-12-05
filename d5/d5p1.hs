
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Text (replace, pack, unpack)

data Command = Command Int Int Int deriving Show

translateInput :: String -> ([[Char]],[Command])
translateInput [] = ([[]],[])
translateInput str = (getStacks $ reverse $ splitOn "\n" $ splitStr str !! 0, getCommands $ splitOn "\n" $ splitStr str !! 1)
    where
        splitStr :: String -> [String]
        splitStr str = splitOn "\n\n" str

getStacks :: [String] -> [[Char]]
getStacks (_:str) = map reverse $ map removeEmpty $ transposeString $ map splitStr str
    where
        transposeString :: [String] -> [String]
        transposeString ([]:_) = []
        transposeString x = (map head x) : transposeString (map tail x)
        splitStr :: String -> [Char]
        splitStr str = [str !! (x + 1) | x <- [0,4 .. length str], x + 1 <= length str]
        removeEmpty :: String -> String
        removeEmpty str = unpack $ replace (pack " ") (pack "") (pack str)

getCommands :: [String] -> [Command]
getCommands [] = []
getCommands (x:xs) = getCommand x : getCommands xs
    where
        getCommand :: String -> Command
        getCommand str = Command (read $ splitStr str !! 0) ((read $ splitStr str !! 1) - 1) ((read $ splitStr str !! 2) - 1)
        splitStr :: String -> [String]
        splitStr str = splitOn "," $ unpack $ replace (pack " to ") (pack ",") $ replace (pack " from ") (pack ",") $ replace (pack "move ") (pack "") (pack str)

runSimulation :: ([[Char]],[Command]) -> [[Char]]
runSimulation (stacks, []) = stacks
runSimulation (stacks, (command:commands)) = runSimulation (runCommand command stacks, commands)

runCommand :: Command -> [[Char]] -> [[Char]]
runCommand (Command 0 _ _) stacks = stacks
runCommand (Command amount from to) stacks = runCommand (Command (amount - 1) from to) (replaceInList (tail (stacks !! from)) from (replaceInList ([head (stacks !! from)] ++ (stacks !! to)) to stacks))

replaceInList :: a -> Int -> [a] -> [a]
replaceInList _ _ [] = []
replaceInList y 0 (_:xs) = y:xs
replaceInList y i (x:xs) = x : (replaceInList y (i - 1) xs)

getPart1 :: [[Char]] -> String
getPart1 xs = map head xs

main = do
        handle <- openFile "d5/d5.txt" ReadMode
        contents <- hGetContents handle
        print $ getPart1 $ runSimulation $ translateInput contents
        hClose handle
