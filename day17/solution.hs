import Split

---------------
-- Mechanics --

data Velocity = V Int Int
instance Show Velocity where show (V u v) = "<" ++ show u ++ ", " ++ show v ++ ">"

data Position = P Int Int deriving Eq
instance Show Position where show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
instance Ord Position where (P _ y1) `compare` (P _ y2) = y1 `compare` y2

data Range = R Int Int
instance Show Range where show (R low high) = show low ++ ".." ++ show high

data Box = B Range Range
instance Show Box where show (B xr yr) = "{x=" ++ show xr ++ ", y=" ++ show yr ++ "}"

origin = P 0 0

half :: Int -> Int
half n
    | even n    = n `div` 2
    | otherwise = error $ "Can't evenly halve " ++ (show n)

position :: Velocity -> Int -> Position
position (V vx0 vy0) n = P
     (n * vx0 - (half $ n * (n-1) * (signum vx0)))
     (n * vy0 - (half $ n * (n-1)))

arc :: Velocity -> [Position]
arc v = map (position v) [0..]

inRange (R min max) u = min <= u && u <= max

inBox (B xr yr) (P x y) = inRange xr x && inRange yr y

outsideBox b = not . inBox b

-- Top of an arcing path. Once it starts to fall, the previous point was the apex.
apex (p0:p1:ps)
    | p0 > p1   = p0
    | otherwise = apex (p1:ps)

-----------------------
-- Input and parsers --

sampleInput = "target area: x=20..30, y=-10..-5"

parse :: String -> Box
parse input = B (parseRange xr) (parseRange yr)
    where [_, ranges] = split ": " input
          [xr, yr] = split ", " ranges

parseRange (_:'=':r) = R (read low) (read high)
    where [low, high] = split ".." r

------------
-- Search --

data Assignment = A Velocity Int

works (A v0 n) t = inBox t $ position v0 n


