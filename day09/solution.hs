import Debug.Trace
import Data.List (intercalate)
import System.IO (putStrLn)
import Split

data Grid = Grid Int Int (Int -> Int -> Int)

instance Show Grid where
    show (Grid m n g) = intercalate "\n" $ [intercalate "" [show $ g i j | j <- [0..n-1]] | i <- [0..m-1]]

makeGrid str = Grid rows cols accessor
    where lines = nlsplit str
          rows = length lines
          cols = length $ lines !! 0
          accessor i j = read [lines !! i !! j] :: Int

boundsCheck m n (i, j) = i >= 0 && j >= 0 && i < m && j < n

neighbors (Grid m n _) i j = filter (boundsCheck m n) [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

isLocalMinimum grid@(Grid _ _ h) (i, j) = all (\(ni, nj) -> h ni nj > h i j) $ neighbors grid i j

allLocalMinima grid@(Grid m n h) = filter (isLocalMinimum grid) [(i, j) | i <- [0..m-1], j <- [0..n-1]]

part1 grid@(Grid _ _ h) = sum $ map ((+1) . uncurry h) $ allLocalMinima grid

bfs :: Grid -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
bfs grid [] visited = visited
bfs grid@(Grid _ _ g) (q@(qi, qj):queue) visited
    | g qi qj == 9 = bfs grid queue visited
    | otherwise    = q : bfs grid newQueue (q : visited)
    where todo loc = not $ elem loc $ queue ++ visited
          newNeighbors = filter todo $ neighbors grid qi qi
          newQueue = queue ++ newNeighbors

bfsFrom :: Grid -> (Int, Int) -> [(Int, Int)]
bfsFrom grid start = bfs grid [start] []

main = do
    input <- readFile "sample.txt"
    let grid = makeGrid input
    putStrLn $ show grid
    putStrLn $ "sum of local minima heights + 1 = " ++ (show $ part1 grid)
    let mins = allLocalMinima grid
    putStrLn $ show $ mins !! 0
    putStrLn $ show $ bfsFrom grid $ mins !! 0
