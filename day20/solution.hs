import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Split

data Pixel = Dark | Light deriving Eq
instance Show Pixel where
    show Dark = "."
    show Light = "#"

toPixel '.' = Dark
toPixel '#' = Light

type Image = Map (Int, Int) Pixel

pixelAt :: (Int, Int) -> Image -> Pixel
pixelAt = Map.findWithDefault Dark

countLight = length . (filter (== Light)) . Map.elems

parsePixels = map toPixel

parseImage lines = Map.fromList [ ((i, j), p i j) | i <- [0..m-1], j <- [0..n-1] ]
    where rows = map parsePixels lines
          m = length lines
          n = length $ rows !! 0
          p i j = (rows !! i) !! j

parseInput input = (parsePixels algoLine, parseImage imageLines)
    where algoLine:"":imageLines = nlsplit input

parseFile filename = do
    input <- readFile filename
    return $ parseInput input