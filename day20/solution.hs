import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
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

neighborhood :: Int -> Int -> Image -> [Pixel]
neighborhood i j image = [ pixelAt (i+di, j+dj) image | di <- [-1,0,1], dj <- [-1,0,1] ]
--    neighborhoodSampleTest 0 0 "....#.#.."

toBit :: Pixel -> Int
toBit Light = 1
toBit Dark = 0

numberAt :: Int -> Int -> Image -> Int
numberAt i j image = foldl (\n b -> 2 * n + b) 0 $ map toBit neighborPixels
    where neighborPixels = neighborhood i j image :: [Pixel]

