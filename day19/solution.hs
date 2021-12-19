import Data.List (intercalate)
import Data.Map (Map, assocs, singleton, union)
import qualified Data.Map as Map
import Data.Maybe

import LCS
import Split
import Sort

data Scanner = Scanner Int [Vector]
instance Show Scanner where
    show (Scanner i bs) = "\nScanner " ++ show i ++ "\n" ++ (intercalate "\n" $ map show bs)

data Vector = V Int Int Int deriving (Eq, Show)

-------------
-- Parsing --

parse :: String -> [Scanner]
parse input = parseScanners $ nlsplit input

parseScanners :: [String] -> [Scanner]
parseScanners lines = map parseScanner $ split [""] lines

parseScanner :: [String] -> Scanner
parseScanner (titleRow:beaconRows) =
    Scanner (parseScannerTitleRow titleRow) $ map parseBeaconPosition beaconRows

parseScannerTitleRow titleRow = read index
    where ["---", "scanner", index, "---"] = words titleRow

parseBeaconPosition line = V (read x) (read y) (read z)
    where [x,y,z] = split "," line

--------------
-- Geometry -- 

minus (V ux uy uz) (V vx vy vz) = V (ux-vx) (uy-vy) (uz-vz)

dot (V ux uy uz) (V vx vy vz) = (ux * vx) + (uy * vy) + (uz * vz)

mag2 u = dot u u

dist2 u v = mag2 $ minus u v

-----------------
-- Unification -- 

play filename = do
    input <- readFile filename
    let scanners = parse input
    putStrLn $ intercalate "\n" $ map show scanners