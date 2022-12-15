
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Text (replace, pack, unpack)

-- Defining Data Types
data Area = Sensor | Beacon | Empty | Signal
    deriving (Eq, Show, Read)
type World = ([[Area]], (Int, Int))

type SensorBeacon = ((Int, Int), (Int, Int))

-- Translating Input
translateInput :: String -> [SensorBeacon]
translateInput [] = []
translateInput str = [(\[s, b] -> ((\[x, y] -> (read x, read y)) $ splitOn ", y=" s,(\[x, y] -> (read x, read y)) $ splitOn ", y=" b)) $ splitOn ": closest beacon is at x=" line | line <- lines $ unpack $ replace (pack "Sensor at x=") (pack "") (pack str)]

makeWorld :: [SensorBeacon] -> World
makeWorld sensorBeacons = ([[Empty | _ <- [minimum $ map fst $ concatMap (\(s, b) -> [s, b]) sensorBeacons .. maximum $ map fst $ concatMap (\(s, b) -> [s, b]) sensorBeacons]] | _ <- [minimum $ map snd $ concatMap (\(s, b) -> [s, b]) sensorBeacons .. maximum $ map snd $ concatMap (\(s, b) -> [s, b]) sensorBeacons]], (minimum $ map fst $ concatMap (\(s, b) -> [s, b]) sensorBeacons, minimum $ map snd $ concatMap (\(s, b) -> [s, b]) sensorBeacons))

-- Helper Functions
nreplace :: Int -> a -> [a] -> [a]
nreplace _ _ [] = []
nreplace n v (x:xs) | n == 0 = v:xs
                    | otherwise = x:nreplace (n-1) v xs

nnreplace :: Int -> Int -> a -> [[a]] -> [[a]]
nnreplace x y v w = nreplace y (nreplace x v (w !! y)) w

-- World Editting
placeSensorBeacon :: [SensorBeacon] -> World -> World
placeSensorBeacon [] world = world
placeSensorBeacon (((sx, sy), (bx, by)):xs) (world, (x, y)) = placeSensorBeacon xs (nnreplace (bx - x) (by - y) Beacon (nnreplace (sx - x) (sy - y) Sensor world), (x, y))

placeSignal :: (Int, Int) -> Int -> World -> World
placeSignal (sx, sy) strength (world, (x, y)) | strength <= 0 = (world, (x, y)) -- For when signal has run out
                                              | (sx - x) >= length (head world) || (sx - x) < 0 || (sy - y) >= length world || (sy - y) < 0 = (world, (x, y)) -- Protecting against index errors
                                              | (world !! (sy - y)) !! (sx - x) /= Empty = placeSignal (sx, sy + 1) (strength - 1) (placeSignal (sx, sy - 1) (strength - 1) (placeSignal (sx + 1, sy) (strength - 1) (placeSignal (sx - 1, sy) (strength - 1) (world, (x, y))))) -- For spanning out signal on marked squares
                                              | otherwise = placeSignal (sx, sy + 1) (strength - 1) (placeSignal (sx, sy - 1) (strength - 1) (placeSignal (sx + 1, sy) (strength - 1) (placeSignal (sx - 1, sy) (strength - 1) (nnreplace (sx - x) (sy - y) Signal world, (x, y))))) -- For spanning out signal on non-marked squares

-- Printing
sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ [] = return ()
sequenceIO_ (x:xs) = do {x; sequenceIO_ xs}

ppWorld :: World -> IO ()
ppWorld (world, _) = sequenceIO_ [sequenceIO_ (putChar '\n':[printer | area <- row, (areaType, printer) <- [(Signal, putChar '#'), (Sensor, putChar 'S'), (Empty, putChar '.'), (Beacon, putChar 'B')], areaType == area]) | row <- world]

main = do
        handle <- openFile "d15/d15.txt" ReadMode
        contents <- hGetContents handle
        ppWorld $ placeSignal (5, 5) 3 $ placeSensorBeacon (translateInput contents) (makeWorld $ translateInput contents)
        hClose handle

-- While this does draw pretty pictures, it is very slow
