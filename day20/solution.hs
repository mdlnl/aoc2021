import Data.List (intercalate)
import Data.Map (Map, keys)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Split
import Testing

type Pixel = Char

light = '#'
dark = '.'

data Image = Image { minRow  :: Int
                   , minCol  :: Int
                   , maxRow  :: Int
                   , maxCol  :: Int
                   , pixels  :: Map (Int, Int) Pixel
                   , outside :: Pixel
                   }

pixelAt :: (Int, Int) -> Image -> Pixel
pixelAt ij@(i, j) image
    | i < minRow image || i > maxRow image || j < minCol image || j > maxCol image = outside image
    | otherwise = Map.findWithDefault dark ij $ pixels image

countLight = length . (filter (== light)) . Map.elems . pixels

boundingBoxPoints border image =
        [ (i,j) | i <- [(minRow image) - border .. (maxRow image) + border],
                  j <- [(minCol image) - border .. (maxCol image) + border] ]

-------------
-- Parsing --

parseImage lines = Image { minRow=0
                         , minCol=0
                         , maxRow=m-1
                         , maxCol=n-1
                         , outside = dark
                         , pixels = Map.fromList [ ((i, j), p i j) | i <- [0..m-1], j <- [0..n-1] ] }
    where m = length lines
          n = length $ lines !! 0
          p i j = (lines !! i) !! j

parseInput input = (algoLine, parseImage imageLines)
    where algoLine:"":imageLines = nlsplit input

parseFile filename = do
    input <- readFile filename
    return $ parseInput input

------------------
-- Neighborhood --

neighborhood :: (Int, Int) -> Image -> [Pixel]
neighborhood (i, j) image = [ pixelAt (i+di, j+dj) image | di <- [-1,0,1], dj <- [-1,0,1] ]

testNeighborhood2 = doTests action [
                                     TC (0, 0) "........#",
                                     TC (1, 1) "....#....",
                                     TC (2, 2) "#........"
                                   ]
    where image = parseImage ["...", ".#.", "..."]
          action (TC ij e) = expect ("neighborhood (center light) " ++ show ij) e
                           $ neighborhood ij image

testNeighborhood3 = doTests action [
                                     TC (0, 0) ".....#.##",
                                     TC (1, 1) ".#.###.#.",
                                     TC (2, 2) "##.#....."
                                   ]
    where image = parseImage [".#.", "###", ".#."]
          action (TC ij e) = expect ("numberAt (plus) " ++ show ij) e
                           $ neighborhood ij image

testNeighborhoodSample = doTests action [
                                            TC (5, 5) "#........"
                                        ]
    where action (TC ij e) = expect ("neighborood (sample) " ++ show ij) e
                           $ neighborhood ij sampleImage0

toBit :: Pixel -> Int
toBit '#' = 1
toBit '.' = 0

numberAt :: (Int, Int) -> Image -> Int
numberAt ij image = foldl (\n b -> 2 * n + b) 0 $ map toBit neighborPixels
    where neighborPixels = neighborhood ij image :: [Pixel]

testNumberAt1 = doTests action [
                                   TC (0,0) 0,
                                   TC (1,1) 0,
                                   TC (2,2) 0
                               ]
    where image = parseImage ["...", "...", "..."]
          action (TC ij e) = expect ("numberAt (all dark) " ++ show ij) e
                           $ numberAt ij image

testNumberAt2 = doTests action [
                                 TC (0, 0) 1,
                                 TC (1, 1) 16,
                                 TC (2, 2) 256
                               ]
    where image = parseImage ["...", ".#.", "..."]
          action (TC ij e) = expect ("numberAt (center light) " ++ show ij) e
                           $ numberAt ij image

testNumberAt3 = doTests action [
                                TC (0, 0) $ 1 + 2 + 8,
                                TC (1, 1) $ 2 + 8 + 16 + 32 + 128,
                                TC (2, 2) $ 32 + 128 + 256
                               ]
    where image = parseImage [".#.", "###", ".#."]
          action (TC ij e) = expect ("numberAt (plus) " ++ show ij) e
                           $ numberAt ij image

testNumberAtSample = doTests action [
                                        TC (5, 5) 256
                                    ]
    where action (TC ij e) = expect ("numberAt (sample) " ++ show ij) e
                           $ numberAt ij sampleImage0

------------
-- Update --

newPixelIsLight algo currentImage ij = algo !! n == '#'
    where n = numberAt ij currentImage

stepImage :: [Pixel] -> Image -> Image
stepImage algo currentImage = Image { minRow = (minRow currentImage) - 1
                                    , minCol = (minCol currentImage) - 1
                                    , maxRow = (maxRow currentImage) + 1
                                    , maxCol = (maxRow currentImage) + 1
                                    , outside = algo !! (numberAt outsideIJ currentImage)
                                    , pixels = Map.fromList [ (p, '#') | p <- newLightPixels ] }
    where newImageLocations = boundingBoxPoints 1 currentImage
          newLightPixels = filter (newPixelIsLight algo currentImage) newImageLocations
          outsideIJ = (maxRow currentImage + 1, maxCol currentImage + 1)
          newOutside = algo !! (numberAt outsideIJ currentImage)

-- testStepImageSample = doTests action [
--                             TC (5, 5, sampleImage0) (7, 7, sampleImage1)
--                         ]
--     where action (TC image 

multistepImage :: [Pixel] -> Image -> Int -> Image
multistepImage algo currentImage 0 = currentImage
multistepImage algo currentImage n = multistepImage algo nextImage (n-1)
    where nextImage = stepImage algo currentImage

showImage image = intercalate "\n" [
        [ pixelAt (i,j) image | j <- [minCol image..maxCol image] ]
                              | i <- [minRow image..maxRow image]
    ]

---------------
-- All Tests --

sampleAlgo = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#. .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#..... .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"

testSampleAlgo = doTests action [ TC 256 '#' ]
    where action (TC i e) = expect ("algo " ++ show i) e $ sampleAlgo !! i

sampleImage0 = parseImage [ "#..#.",
                            "#....",
                            "##..#",
                            "..#..",
                            "..###" ]

-- Sample after 0 steps:
--   -4   0   4   8
--   ...............
-- -4...............
--   ...............
--   ...............
--   ...............
--  0.....#..#......
--   .....#.........
--   .....##..#.....
--   .......#.......
--  4.......###.....
--   ...............
--   ...............
--   ...............
--  8...............
--   ...............

-- Sample after 1 step:
--   -4   0   4   8
--   ...............
-- -4...............
--   ...............
--   ...............
--   .....##.##.....
--  0....#..#.#.....
--   ....##.#..#....
--   ....####..#....
--   .....#..##.....
--  4......##..#....
--   .......#.#.....
--   ...............
--   ...............
--  8...............
--   ...............

testAll = do
    testSampleAlgo
    testNeighborhood2
    testNeighborhood3
    testNeighborhoodSample
    testNumberAt1
    testNumberAt2
    testNumberAt3
    testNumberAtSample

part1 filename = do
    (algo, image0) <- parseFile filename
    putStrLn $ "Initial:\n" ++ showImage image0
    let image1 = stepImage algo image0
    putStrLn $ "After 1 step:\n" ++ showImage image1
    let image2 = stepImage algo image1
    putStrLn $ "After 2 steps:\n" ++ showImage image2
    putStrLn $ show $ countLight image2
