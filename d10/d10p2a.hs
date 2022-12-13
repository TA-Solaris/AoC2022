import System.IO  
import Control.Monad
main = do
        handle <- openFile "d10/d10.txt" ReadMode
        contents <- hGetContents handle
        putStr $ (let getScreening his = if his == [] then "" else if (fst (head (take 1 his))) - ((snd (head (take 1 his)) - 1) `mod` 40) `elem` [(-1), 0, 1] && (snd (head (take 1 his))) `mod` 40 == 0 then '#':'\n':getScreening (drop 1 his) else if (fst (head (take 1 his))) - ((snd (head (take 1 his)) - 1) `mod` 40) `elem` [(-1), 0, 1] then '#':getScreening (drop 1 his) else if (snd (head (take 1 his))) `mod` 40 == 0 then ' ':'\n':getScreening (drop 1 his) else ' ':getScreening (drop 1 his) in getScreening) $ (\(_, _, his) -> reverse his) $ (let runCommands cmds cpu = if null cmds then cpu else runCommands (drop 1 cmds) ((\cmd (x, c, his) -> if cmd == "noop" then (x, c + 1, (x, c + 1):his) else (\('a':'d':'d':'x':' ':v) (x, c, his) -> (x + read v::Int, c + 2, (x, c + 2):(x, c + 1):his)) cmd (x, c, his)) (head (take 1 cmds)) cpu) in runCommands) (lines contents) (1, 0, [])
        hClose handle