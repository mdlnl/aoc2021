import qualified Data.MultiMap as MM
import Data.List
import Debug.Trace
import Split
import Text.Printf

parseEdge line = (from, to)
    where [from, to] = split "-" line

parseGraph :: [String] -> MM.MultiMap String String
parseGraph [] = MM.empty
parseGraph (line:lines) = MM.insert from to $ MM.insert to from $ parseGraph lines
    where (from, to) = parseEdge line

maybeAddSmall :: String -> [String] -> [String]
maybeAddSmall cave smalls
    | all (\c -> elem c ['a'..'z']) cave = cave : smalls
    | otherwise = smalls

traverseNoSecondSmall :: MM.MultiMap String String -> [String] -> [String] -> [[String]]
traverseNoSecondSmall _ path@("end":_) _ = [reverse path]
traverseNoSecondSmall graph (h:t) smalls
    | elem h smalls = []
    | otherwise = foldr (++) [] outPaths
    where outEdges = [oe | oe <- MM.lookup h graph, oe /= "start"]
          newSmalls = maybeAddSmall h smalls
          outPaths = [traverseNoSecondSmall graph (oe : h : t) newSmalls | oe <- outEdges]

main = do
    input <- readFile "full.txt"
    let lines = split "\n" input
    let graph = parseGraph lines
    let paths = traverseNoSecondSmall graph ["start"] []
    putStrLn $ show $ length paths
