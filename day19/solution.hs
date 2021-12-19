import Data.List (intercalate)
import Data.Map (Map, assocs, singleton, union)
import qualified Data.Map as Map (lookup)
import Data.Maybe

import LCS
import Split
import Sort

data ScannerReport = Scanner Int [Beacon]
instance Show ScannerReport where
    show (Scanner i bs) = "\nScanner " ++ show i ++ "\n" ++ (intercalate "\n" $ map show bs)

data Vector = V Int Int Int deriving (Eq, Show)

-- Map from scanner index to position in that scanner's frame.
type RelativePosition = Map Int Vector

-- Each beacon tracks its position relative to every scanner
data Beacon = Beacon RelativePosition
instance Show Beacon where
    show (Beacon rpmap) = "{" ++ intercalate " ; " [
        show i ++ "->(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"
        | (i, V x y z) <- assocs rpmap] ++ "}"

posIn i (Beacon rpmap) = Map.lookup i rpmap

-------------
-- Parsing --

parse :: String -> [ScannerReport]
parse input = parseScannerReports $ nlsplit input

parseScannerReports :: [String] -> [ScannerReport]
parseScannerReports lines = map parseScannerReport $ split [""] lines

parseScannerReport :: [String] -> ScannerReport
parseScannerReport (titleRow:beaconRows) = Scanner index [
        Beacon $ singleton index position
        | position <- map parseBeaconPosition beaconRows ]
    where index = parseScannerTitleRow titleRow

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

pairwiseSquaredDistances (Scanner scannerIndex beacons) = quicksort (\(d,_,_) -> d) [
    (dist2 u v, u, v) | i <- [0..n-1], j <- [i+1..n-1], u <- [bp i], v <- [bp j] ]
    where posFn = posIn scannerIndex
          beaconPositions = map (fromJust . posFn) beacons
          n = length beaconPositions
          bp = (beaconPositions !!)

-----------------
-- Unification -- 

unify (Beacon b) (Beacon c) = Beacon (union b c)

play filename = do
    input <- readFile filename
    let scanners = parse input
    let psds = map (show . pairwiseSquaredDistances) scanners
    putStrLn $ intercalate "\n" $ psds
