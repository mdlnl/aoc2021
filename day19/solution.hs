import Data.List (intercalate)
import Data.Map (Map, assocs, singleton)
import Split

data ScannerReport = Scanner Int [Beacon]
instance Show ScannerReport where
    show (Scanner i bs) = "Scanner " ++ show i ++ "\n" ++ (intercalate "\n" $ map show bs)

data Vector = V Int Int Int deriving (Eq, Show)

-- Map from scanner index to position in that scanner's frame.
type RelativePosition = Map Int Vector

-- Each beacon tracks its position relative to every scanner
data Beacon = Beacon RelativePosition
instance Show Beacon where
    show (Beacon rpmap) = "{" ++ intercalate " ; " [
        show i ++ "->(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"
        | (i, V x y z) <- assocs rpmap] ++ "}"

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