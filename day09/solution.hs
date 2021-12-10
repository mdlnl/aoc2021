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

neighbors (Grid m n _) i j = filter (\(ni, nj) -> ni >= 0 && ni < m && nj >= 0 && nj < n) [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

isLocalMinimum grid@(Grid _ _ h) (i, j) = all (\(ni, nj) -> h ni nj > h i j) $ neighbors grid i j

allLocalMinima grid@(Grid m n h) = filter (isLocalMinimum grid) [(i, j) | i <- [0..m-1], j <- [0..n-1]]

part1 grid@(Grid _ _ h) = sum $ map ((+1) . uncurry h) $ allLocalMinima grid

main = do
    input <- readFile "sample.txt"
    let grid = makeGrid input
    putStrLn $ show grid
    putStrLn $ show $ allLocalMinima grid
    putStrLn $ show $ part1 grid