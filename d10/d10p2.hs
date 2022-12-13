
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

absDiff :: Num a => a -> a -> a
absDiff n = abs . (n-)  

getScreening :: His -> String
getScreening ((x, c):his) | absDiff x ((c - 1) `mod` 40) <= 1 && c `mod` 40 == 0 = '#':'\n':getScreening his
                          | absDiff x ((c - 1) `mod` 40) <= 1 = '#':getScreening his
                          | c `mod` 40 == 0 = ' ':'\n':getScreening his
                          | otherwise = ' ':getScreening his
getScreening [] = ""

-- Part2
part2 :: [Command] -> String
part2 cmds = getScreening $ getHistory $ runCommands cmds (1, 0, [])

main = do
        handle <- openFile "d10/d10.txt" ReadMode -- I've left this input as my input because it looks cool
        contents <- hGetContents handle
        putStr $ part2 $ translateInput contents
        hClose handle
