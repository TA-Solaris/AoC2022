import System.IO  
import Control.Monad
main = do
        handle <- openFile "d10/d10.txt" ReadMode
        contents <- hGetContents handle
        putStr $ (let getScreening his = if his == [] then "" else (if ((fst ((take 1 his) !! 0)) - (((snd ((take 1 his) !! 0)) `mod` 40) - 1)) `elem` [(-1), 0, 1] && (snd ((take 1 his) !! 0)) `mod` 40 == 0 then '#':'\n':(getScreening (drop 1 his)) else (if ((fst ((take 1 his) !! 0)) - (((snd ((take 1 his) !! 0)) `mod` 40) - 1)) `elem` [(-1), 0, 1] then '#':(getScreening (drop 1 his)) else (if (snd ((take 1 his) !! 0)) `mod` 40 == 0 then '.':'\n':(getScreening (drop 1 his)) else '.':(getScreening (drop 1 his))))) in getScreening) $ (\(_, _, his) -> reverse his) $ (let runCommands cmds cpu = if cmds == [] then cpu else runCommands (drop 1 cmds) ((\cmd (x, c, his) -> if cmd == "noop" then (x, c + 1, ((x, c + 1):his)) else (\('a':'d':'d':'x':' ':v) (x, c, his) -> (x + (read v)::Int, c + 2, ((x, c + 2):(x, c + 1):his))) cmd (x, c, his)) ((take 1 cmds) !! 0) cpu) in runCommands) (lines contents) (1, 0, [])
        hClose handle