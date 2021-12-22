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

neighborhood :: (Int, Int) -> Image -> [Pixel]
neighborhood (i, j) image = [ pixelAt (i+di, j+dj) image | di <- [-1,0,1], dj <- [-1,0,1] ]

toBit :: Pixel -> Int
toBit Light = 1
toBit Dark = 0

numberAt :: (Int, Int) -> Image -> Int
numberAt ij image = foldl (\n b -> 2 * n + b) 0 $ map toBit neighborPixels
    where neighborPixels = neighborhood ij image :: [Pixel]

stepPixel algo ij image = algo !! (numberAt ij image)

updateImage image Light ij = Map.insert ij Light image
updateImage image Dark ij  = Map.delete ij       image

stepImage algo (rows, cols, image) = (newRows, newCols, image)
    where newRows = rows - 2
          newCols = cols - 2
          newImageLocations = [ (i,j) | i <- [(-1)..newRows-1],
                                        j <- [(-1)..newCols-1] ]
          newImage = foldr (stepPixel algo) image newImageLocations

multistepImage algo state 0 = state
multistepImage algo state n = multistepImage algo nextState (n-1)
    where nextState = stepImage algo state
