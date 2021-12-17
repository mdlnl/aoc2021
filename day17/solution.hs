import Split

---------------
-- Mechanics --

data Velocity = V Int Int
instance Show Velocity where show (V u v) = "<" ++ show u ++ ", " ++ show v ++ ">"

data Position = P Int Int
instance Show Position where show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

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


