import qualified Data.MultiMap as MM
import Data.List (elem)
import Data.Maybe
import Data.Set (Set, empty, fromList, insert, member)
import Debug.Trace
import Split
import Text.Printf

type Graph = MM.MultiMap String String
type Node = String
type Path = [Node]
type Nodes = Set Node

parseGraph :: [String] -> Graph
parseGraph [] = MM.empty
parseGraph (line:lines) = MM.insert from to $ MM.insert to from $ parseGraph lines
    where [from, to] = split "-" line

isSmall c = member c $ fromList ['a'..'z']

maybeAddSmall :: Node -> Nodes -> Nodes
maybeAddSmall cave smallCaves
    | all (\c -> elem c ['a'..'z']) cave = insert cave smallCaves
    | otherwise = smallCaves

outEdges :: Graph -> Node -> [Node]
outEdges graph v = [oe | oe <- MM.lookup v graph, oe /= "start"]

-- traverseNoSecondSmall graph pathPrefix smalls -> [path]
traverseNoSecondSmall :: Graph -> Path -> Nodes -> [Path]
traverseNoSecondSmall _ path@("end":_) _ = [reverse path]
traverseNoSecondSmall graph (h:t) smalls
    | member h smalls = []
    | otherwise     = foldr (++) [] outPaths
    where newSmalls = maybeAddSmall h smalls
          outPaths = [traverseNoSecondSmall graph (oe : h : t) newSmalls | oe <- outEdges graph h]

part1 :: String -> Int
part1 input = length $ traverseNoSecondSmall graph ["start"] empty
    where graph = parseGraph $ split "\n" input

-- traverseOneSecondSmall graph pathPrefix smalls secondSmall -> [path]
traverseOneSecondSmall :: Graph -> Path -> Nodes -> Maybe Node -> [Path]
traverseOneSecondSmall _ path@("end":_) _ _ = [reverse path]
traverseOneSecondSmall graph (h:t) smalls ss@(Just _)
    | member h smalls = []
    | otherwise     = foldr (++) [] outPaths
    where newSmalls = maybeAddSmall h smalls
          outPaths = [
            traverseOneSecondSmall graph (oe : h : t) newSmalls ss | oe <- outEdges graph h]
traverseOneSecondSmall graph (h:t) smalls Nothing
    | member h smalls = foldr (++) [] [
            traverseOneSecondSmall graph (oe : h : t) newSmalls (Just h) | oe <- outEdges graph h]
    | otherwise     = foldr (++) [] [
            traverseOneSecondSmall graph (oe : h : t) newSmalls Nothing | oe <- outEdges graph h]
    where newSmalls = maybeAddSmall h smalls

part2 :: String -> Int
part2 input = length $ traverseOneSecondSmall graph ["start"] empty Nothing
    where graph = parseGraph $ split "\n" input

main = do
    sample <- readFile "sample.txt"
    full <- readFile "full.txt"
    putStrLn $ "part1(sample): " ++ (show $ part1 sample)
    putStrLn $ "part1(full): " ++ (show $ part1 full)
    putStrLn $ "part2(sample): " ++ (show $ part2 sample)
    putStrLn $ "part2(full): " ++ (show $ part2 full)
