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

parseInput input = (parsePixels algoLine, rows, cols, parseImage imageLines)
    where algoLine:"":imageLines = nlsplit input
          rows = length imageLines
          cols = length $ imageLines !! 0

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

stepPixel :: [Pixel] -> (Int, Int) -> Image -> Image
stepPixel algo ij image = updateImage image ij $ algo !! (numberAt ij image)

updateImage image ij Light = Map.insert ij Light image
updateImage image ij Dark  = Map.delete ij       image

type State = (Int, Int, Image)

stepImage :: [Pixel] -> State -> State
stepImage algo (rows, cols, image) = (newRows, newCols, newImage)
    where newRows = rows - 2
          newCols = cols - 2
          newImageLocations = [ (i,j) | i <- [(-1)..newRows-1],
                                        j <- [(-1)..newCols-1] ]
          newImage = foldr (stepPixel algo) image newImageLocations

multistepImage :: [Pixel] -> State -> Int -> State
multistepImage algo state 0 = state
multistepImage algo state n = multistepImage algo nextState (n-1)
    where nextState = stepImage algo state

part1 filename = do
    (algo, rows, cols, image) <- parseFile filename
    let (_,_,finalImage) = multistepImage algo (rows, cols, image) 2
    putStrLn $ show $ countLight finalImage