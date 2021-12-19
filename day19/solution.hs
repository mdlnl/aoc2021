import Data.List (intercalate)
import Data.Map (Map, assocs, singleton, union)
import qualified Data.Map as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
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

-- Scanner index and beacon index within scanner report
data BeaconId = BID Int Int

-- relative position of each beacon
type ScannerMap = Map BeaconId Vector

-- with beacon equivalence map
type BeaconEquivalence = MultiMap BeaconId BeaconId

data BeaconData = BD ScannerMap BeaconEquivalence

-------------
-- Parsing --

parse :: String -> [ScannerReport]
parse input = arseScannerReports $ nlsplit input

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
    (dist2 u v, b i, b j) | i <- [0..n-1], j <- [i+1..n-1], u <- [bp i], v <- [bp j] ]
    where posFn = posIn scannerIndex
          beaconPositions = map (fromJust . posFn) beacons
          n = length beaconPositions
          b = (beacons !!)
          bp = (beaconPositions !!)

-----------------
-- Unification -- 

unify (Beacon b) (Beacon c) = Beacon (union b c)

-- Unify beacon "edges" between two scanner reports; we don't know which is which yet.
edgeUnify (d1, b1, b2) (d2, c1, c2)
    | d1 /= d2  = error "Can't unify edges of different lengths."
    | otherwise = Either (unify b1 c1, unify b2 b2) (unify b1 c2, unify b2 c1)

play filename = do
    input <- readFile filename
    let scanners = parse input
    let psds = map (show . pairwiseSquaredDistances) scanners
    ---let common = lcsWith (\(a,_,_) (b,_,_) -> a == b) 
    putStrLn $ intercalate "\n" $ psds