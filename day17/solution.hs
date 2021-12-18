import Data.Ratio
import Split

-------------
-- Geomtry --

data Position = P Int Int deriving Eq
instance Show Position where show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
-- useful for finding apex
instance Ord Position where (P _ y1) `compare` (P _ y2) = y1 `compare` y2

data Range = R Int Int
instance Show Range where show (R low high) = show low ++ ".." ++ show high

data Box = B Range Range
instance Show Box where show (B xr yr) = "{x=" ++ show xr ++ ", y=" ++ show yr ++ "}"

inRange (R min max) u = min <= u && u <= max

inBox (B xr yr) (P x y) = inRange xr x && inRange yr y

---------------
-- Mechanics --

data Velocity = V Int Int deriving Eq
instance Show Velocity where show (V u v) = "<" ++ show u ++ ", " ++ show v ++ ">"
-- useful for finding highest arc
instance Ord Velocity where
    u `compare` v = aa u `compare` aa v
        where aa = apex . arc

half :: Int -> Int
half n
    | even n    = n `div` 2
    | otherwise = error $ "Can't evenly halve " ++ (show n)

xpos v0 n = nx * v0 - (half $ nx * (nx-1) * (signum v0))
     where nx = min n $ abs v0
ypos v0 n = n * v0 - (half $ n * (n-1))

xarc v0 = map (xpos v0) [0..]

position :: Velocity -> Int -> Position
position (V vx0 vy0) n = P (xpos vx0 n) (ypos vy0 n)

arc :: Velocity -> [Position]
arc v = map (position v) [0..]

-- Top of an arcing path. Once it starts to fall, the previous point was the apex.
apex (p0:p1:ps)
    | p0 > p1   = p0
    | otherwise = apex (p1:ps)

apexStarting = apex . arc

-----------------------
-- Input and parsers --

sampleInput = "target area: x=20..30, y=-10..-5"
sampleTarget = parse sampleInput
shouldEverWorkForSample  = [V 7 2, V 6 3, V 9 0]
shouldNeverWorkForSample = [V 17 (-14)]

realInput = "target area: x=135..155, y=-102..-78"
realTarget = parse realInput

parse :: String -> Box
parse input = B (parseRange xr) (parseRange yr)
    where [_, ranges] = split ": " input
          [xr, yr] = split ", " ranges

parseRange (_:'=':r) = R (read low) (read high)
    where [low, high] = split ".." r

------------
-- Search --

untilFixedPoint :: Eq b => (a -> b) -> [a] -> [a]
untilFixedPoint f [] = []
untilFixedPoint f (a:b:as)
    | f a == f b = [a]
    | otherwise  = a : (untilFixedPoint f $ b:as)

everInXRange xr@(R left right) vx0
    | 0 < left && vx0 > 0 = not $ null $ filter (inRange xr) $ untilFixedPoint id $ xarc vx0
    | otherwise           = error "require 0 < left and vx0 > 0"

stepsInXrange xr@(R left _) vx0
    | everInXRange xr vx0 = takeWhile inxr $ dropWhile (not . inxr) [1..]
    | otherwise           = []
    where x = xpos vx0
          inxr = inRange xr . xpos vx0

-- returns a potentially-infinite list!
xSearchSpace xr@(R left right)
    | inRange xr 0 = [0]
    | 0 < left     = filter (everInXRange xr) [1..right]
    | otherwise    = error "require left <= 0 <= right or 0 < left"

ySearchSpace yr@(R bottom top)
    | top < 0   = [bottom..(-bottom)]
    | otherwise = error "Require top < 0"

searchSpace b@(B xr yr) = [ V vx0 vy0 | vx0 <- xSearchSpace xr,
                                        vy0 <- ySearchSpace yr ]

stepsOnTarget t@(B xr yr@(R bottom _)) v0@(V vx0 _) =
    filter inside $ takeWhile above $ stepsInXrange xr vx0
    where p = position v0
          inside = inBox t . p
          above n = p n >= (P 0 bottom)

everOnTarget t v0 = not $ null $ stepsOnTarget t v0

search target = filter (everOnTarget target) $ searchSpace target

part1 = apexStarting . maximum . search

part2 = length . search
