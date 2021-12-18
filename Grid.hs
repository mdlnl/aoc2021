import Data.List (intercalate)
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

day09_bfs :: Grid -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
day09_bfs grid [] visited = visited
day09_bfs grid@(Grid _ _ g) (q@(qi, qj):queue) visited
    | g qi qj == 9 = day09_bfs grid queue visited
    | otherwise    = q : day09_bfs grid newQueue (q : visited)
    where todo loc = not $ elem loc $ queue ++ visited
          newNeighbors = filter todo $ neighbors grid qi qi
          newQueue = queue ++ newNeighbors

day09_bfsFrom :: Grid -> (Int, Int) -> [(Int, Int)]
day09_bfsFrom grid start = day09_bfs grid [start] []
