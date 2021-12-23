import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Split

type Pixel = Char

light = '#'
dark = '.'

type Image = Map (Int, Int) Pixel

pixelAt :: (Int, Int) -> Image -> Pixel
pixelAt = Map.findWithDefault dark

countLight = length . (filter (== light)) . Map.elems

-------------
-- Parsing --

parseImage lines = Map.fromList [ ((i, j), p i j) | i <- [0..m-1], j <- [0..n-1] ]
    where m = length lines
          n = length $ lines !! 0
          p i j = (lines !! i) !! j

parseInput input = (algoLine, rows, cols, parseImage imageLines)
    where algoLine:"":imageLines = nlsplit input
          rows = length imageLines
          cols = length $ imageLines !! 0

parseFile filename = do
    input <- readFile filename
    return $ parseInput input

------------------
-- Neighborhood --

neighborhood :: (Int, Int) -> Image -> [Pixel]
neighborhood (i, j) image = [ pixelAt (i+di, j+dj) image | di <- [-1,0,1], dj <- [-1,0,1] ]

toBit :: Pixel -> Int
toBit '#' = 1
toBit '.' = 0

numberAt :: (Int, Int) -> Image -> Int
numberAt ij image = foldl (\n b -> 2 * n + b) 0 $ map toBit neighborPixels
    where neighborPixels = neighborhood ij image :: [Pixel]

stepPixel :: [Pixel] -> (Int, Int) -> Image -> Image
stepPixel algo ij image = updateImage image ij $ algo !! (numberAt ij image)

updateImage image ij '#' = Map.insert ij '#' image
updateImage image ij '.' = Map.delete ij     image

type State = (Int, Int, Image)

stepImage :: [Pixel] -> State -> State
stepImage algo (rows, cols, image) = (newRows, newCols, newImage)
    where newRows = rows + 2
          newCols = cols + 2
          newImageLocations = [ (i,j) | i <- [(-1)..newRows-1],
                                        j <- [(-1)..newCols-1] ]
          newImage = foldr (stepPixel algo) image newImageLocations

multistepImage :: [Pixel] -> State -> Int -> State
multistepImage algo state 0 = state
multistepImage algo state n = multistepImage algo nextState (n-1)
    where nextState = stepImage algo state

showImage image minRow maxRow minCol maxCol = intercalate "\n" [
        [ pixelAt (i,j) image | j <- [minCol..maxCol] ]
                              | i <- [minRow..maxRow]
    ]

part1 filename = do
    (algo, rows0, cols0, image0) <- parseFile filename
    let state0 = (rows0, cols0, image0)
    putStrLn $ "Initial:\n" ++ showImage image0 (-1) 5 (-1) 5
    let state1@(rows1, cols1, image1) = stepImage algo state0
    putStrLn $ "After 1 step:\n" ++ showImage image1 (-2) 6 (-2) 6
    let state2@(rows2, cols2, image2) = stepImage algo state1
    putStrLn $ "After 2 steps:\n" ++ showImage image2 (-3) 7 (-3) 7
    putStrLn $ show $ countLight image2
