import Data.Map (Map)
import qualified Data.Map as Map

data Pixel = Dark | Light deriving Eq
instance Show Pixel where
    show Dark = "."
    show Light = "#"

type Image = Map (Int, Int) -> Pixel

pixelAt = Map.findWithDefault Dark

countLight = length $ filter (= Light) $ Map.elems

