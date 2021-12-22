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

parseImage lines = 
    where rows = map parsePixels

parseInput input = (parsePixels algoLine, parseImage imageLines)
    where algoLine:"":imageLines = nlsplit input

parseFile filename = do
    input <- readFile filename
    return $ parseInput input