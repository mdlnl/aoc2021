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

--part1 (Grid m n h) = foldr (*) 1 [h i j | i <- [0..m-1], j <- [0..n-1]]

main = do
    input <- readFile "sample.txt"
    putStrLn $ show $ makeGrid input