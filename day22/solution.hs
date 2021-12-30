import Data.List (intercalate)
import Data.Maybe
import Debug.Trace
import Split
import Testing

-----------
-- Types --

data Range = Range { from::Int, to::Int } | EmptyRange deriving Eq
instance Show Range where show (Range f t) = show f ++ ".." ++ show t

middle :: Range -> Int
middle (Range f t) = (f + t) `div` 2

rlen EmptyRange = error "Cannot get length of empty range"
rlen (Range f t)
    | f <= t    = t - f + 1
    | otherwise = 0

data Value = On | Off deriving (Eq, Show)

data RebootStep = RebootStep { value::Value, xrange::Range, yrange::Range, zrange::Range } deriving Eq
instance Show RebootStep where show (RebootStep v xr yr zr) = show v ++ " "
                                                                     ++ "x=" ++ show xr
                                                                     ++ ",y=" ++ show yr
                                                                     ++ ",z=" ++ show zr

type Point = (Int, Int, Int)

type Cuboid = (Range, Range, Range)

bottomCorner :: Cuboid -> Point
bottomCorner (Range xf _, Range yf _, Range zf _) = (xf, yf, zf)

topCorner :: Cuboid -> Point
topCorner (Range _ xt, Range _ yt, Range _ zt) = (xt, yt, zt)

volume :: Cuboid -> Int
volume (xr, yr, zr) = (rlen xr) * (rlen yr) * (rlen zr)

cuboid :: RebootStep -> Cuboid
cuboid (RebootStep _ xr yr zr) = (xr, yr, zr)

center :: Cuboid -> Point
center (xr, yr, zr) = (middle xr, middle yr, middle zr)

data Coord = X | Y | Z deriving (Eq, Show)

dim X (xrange, _, _) = xrange
dim Y (_, yrange, _) = yrange
dim Z (_, _, zrange) = zrange

-------------
-- Parsing --

parseRange labeledRange = Range { from = read fromStr :: Int, to = read toStr :: Int }
    where [_, fromDotDotTo] = split "=" labeledRange
          [fromStr, toStr] = split ".." fromDotDotTo

testParseRange = doTests action [ TC "x=1..2"         $ Range 1 2
                                , TC "x=-1234..-7890" $ Range (-1234) (-7890) ]
    where action (TC i e) = expect "parseRange" e $ parseRange i

parseValue "on" = On
parseValue "off" = Off

parseRebootStep line = RebootStep { value = parseValue value
                                  , xrange = parseRange xrange
                                  , yrange = parseRange yrange
                                  , zrange = parseRange zrange }
    where [value, ranges] = split " " line
          [xrange, yrange, zrange] = split "," ranges

testParseRebootStep = doTests action [
        TC "on x=1..2,y=-100..500,z=-3..-4"
           $ RebootStep On (Range 1 2) (Range (-100) 500) (Range (-3) (-4)),
        TC "off x=1..2,y=-100..500,z=-3..-4"
           $ RebootStep Off (Range 1 2) (Range (-100) 500) (Range (-3) (-4))
    ]
    where action (TC i e) = expect "parseRebootStep" e $ parseRebootStep i

parseInput input = map parseRebootStep $ nlsplit input

parseFile filename = do
    input <- readFile filename
    return $ parseInput input

----------------
-- Insideness --

inRange :: Int -> Range -> Bool
inRange x (Range from to) = from <= x && x <= to

inside :: Point -> RebootStep -> Bool
inside (x, y, z) (RebootStep _ xr yr zr) =  inRange x xr
                                         && inRange y yr
                                         && inRange z zr

testInside = doTests action [ TC ( -1,  20, 40) False
                            , TC (  0,  20, 40) True
                            , TC ( 11,  20, 40) False
                            ]
    where rs = RebootStep On (Range 0 10) (Range 20 30) (Range 40 50)
          action (TC i e) = expect "inside" e $ inside i rs


stateAfter rebootSteps xyz 
    | values == [] = Off
    | otherwise    = last values
    where values = map value $ filter (inside xyz) rebootSteps

firstInside reverseSteps xyz
    | first == [] = Off
    | otherwise   = value $ first !! 0
    where first = take 1 $ filter (inside xyz) reverseSteps


smallestEnclosingRange :: Range -> Range -> Range
smallestEnclosingRange (Range f1 t1) (Range f2 t2) = Range (min f1 f2) (max t1 t2)

smallestEnclosingCuboid :: Cuboid -> Cuboid -> Cuboid
smallestEnclosingCuboid (cx, cy, cz) (dx, dy, dz) = (xr, yr, zr)
    where xr = smallestEnclosingRange cx dx
          yr = smallestEnclosingRange cy dy
          zr = smallestEnclosingRange cz dz

-----------------------------------
-- Tree for storing cuboids in --

nextCoord X = Y
nextCoord Y = Z
nextCoord Z = X

data Bigtree = Node { local  :: Cuboid
                    , branch :: Coord
                    , below  :: Bigtree
                    , inline :: Bigtree
                    , above  :: Bigtree
                    }
             | Empty
             deriving (Eq)

branchInterval :: Bigtree -> Range
-- don't define for Empty and don't call for Empty
branchInterval (Node (xr,_,_) X _ _ _) = xr
branchInterval (Node (_,yr,_) Y _ _ _) = yr
branchInterval (Node (_,_,zr) Z _ _ _) = zr

showBigtree indent Empty = indent ++ "()"
showBigtree indent node  = (show $ local node) ++ "{"
                        ++ showChild "<" below
                        ++ showChild "=" inline
                        ++ showChild ">" above
                        ++ " }"
    where showChild s c = if c node == Empty
                          then ""
                          else "\n" ++ indent ++ (show $ branch node) ++ " " ++ s
                                    ++ showBigtree (' ':' ':indent) (c node)
instance Show Bigtree where show node = "\n" ++ showBigtree "" node ++ "\n"

leaf br cub = Node { local=cub, branch=br, below=Empty, inline=Empty, above=Empty }

nonempty :: Cuboid -> Bool
nonempty cub = volume cub > 0

bigtreeVolume :: Bigtree -> Int
bigtreeVolume Empty = 0
bigtreeVolume node = (volume $ local node)
                   + (bigtreeVolume $ below node)
                   + (bigtreeVolume $ inline node)
                   + (bigtreeVolume $ above node)

leaf510 = leaf X (Range 5 10, Range 50 100, Range 500 1000)

testBigtreeVolume = doTests action [
        TC leaf510 (6 * 51 * 501),
        TC Empty 0 ]
    where action (TC i e) = expect "bigtree volume" e $ bigtreeVolume i

insert :: Coord -> Cuboid -> Bigtree -> Bigtree
-- ignore EmptyRanges generated by taking chunks of a cuboid that aren't there, like the chunk of
-- (10..20,_,_) below (0..5).
insert _ (EmptyRange, _, _) bt = bt
insert _ (_, EmptyRange, _) bt = bt
insert _ (_, _, EmptyRange) bt = bt
insert b c Empty
    | nonempty c = leaf b c
    | otherwise  = Empty
insert _ c@(xr, yr, zr) node@(Node loc bra abo inl bel)
    | nonempty c = Node { local  = loc
                        , branch = bra
                        , below  = insert (nextCoord bra) cuboidChunkBelow bel
                        , inline = insert (nextCoord bra) cuboidChunkInline inl
                        , above  = insert (nextCoord bra) cuboidChunkAbove abo }
    | otherwise  = node
    where cutRange = dim bra $ local node
          cuboidChunkBelow = chunkBelow bra cutRange c
          cuboidChunkInline = chunkInline bra cutRange c
          cuboidChunkAbove = chunkAbove bra cutRange c

testInsertLeaf510 = doTests action [
        -- Insert a cuboid that doesn't overlap with leaf510 
        TC (Range 1 2, Range 50 100, Range 500 1000) $
                Node { local  = (Range 5 10, Range 50 100, Range 500 1000)
                     , branch = X
                     , above  = Empty
                     , inline = Empty
                     , below  = leaf Y (Range 1 2, Range 50 100, Range 500 1000) },
        TC (Range 6 8, Range 50 100, Range 500 1000) $
                Node { local  = (Range 5 10, Range 50 100, Range 500 1000)
                     , branch = X
                     , above  = Empty
                     , inline = leaf Y (Range 6 8, Range 50 100, Range 500 1000)
                     , below  = Empty },
        TC (Range 12 20, Range 50 100, Range 500 1000) $
                Node { local  = (Range 5 10, Range 50 100, Range 500 1000)
                     , branch = X
                     , above  = leaf Y (Range 12 20, Range 50 100, Range 500 1000)
                     , inline = Empty
                     , below  = Empty }
        ]
    where action (TC i e) = expect ("insert " ++ show i ++ " leaf510") e $ insert X i leaf510

withDim X xr (_, yr, zr) = (xr, yr, zr)
withDim Y yr (xr, _, zr) = (xr, yr, zr)
withDim Z zr (xr, yr, _) = (xr, yr, zr)

chunkBelow coord (Range f _) cuboid
    | cf > f    = withDim coord EmptyRange cuboid
    | otherwise = withDim coord (Range cf (min ct f)) cuboid
    where Range cf ct = dim coord cuboid
chunkInline coord (Range f t) cuboid
    | ct < f    = withDim coord EmptyRange cuboid
    | cf > t    = withDim coord EmptyRange cuboid
    | otherwise = withDim coord (Range (max cf f) (min ct t)) cuboid
    where Range cf ct = dim coord cuboid
chunkAbove coord (Range _ t) cuboid
    | ct < t    = withDim coord EmptyRange cuboid
    | otherwise = withDim coord (Range (max cf t) ct) cuboid
    where Range cf ct = dim coord cuboid

-----------
-- Tests --

testAll = do
    testParseRange
    testParseRebootStep
    testInside
    testBigtreeVolume
    testInsertLeaf510

-------------------
-- Problem parts --

cuboid50 = [ (x, y, z) | x <- [-50..50]
                       , y <- [-50..50]
                       , z <- [-50..50] ]

part1 filename = do
    rebootSteps <- parseFile filename
    let reverseSteps = reverse rebootSteps
    let numberOn = length $ filter (==On) $ map (stateAfter rebootSteps) cuboid50
    --let numberOn = length $ filter (==On) $ map (firstInside reverseSteps) cuboid50
    putStrLn $ show numberOn

part2 filename = do
    rebootSteps <- parseFile filename
    let c0 = cuboid $ rebootSteps !! 0
    let cs = drop 1 $ map cuboid $ rebootSteps
    let sec@(xr, yr, zr) = foldl smallestEnclosingCuboid c0 cs
    putStrLn $ show sec
    let secPoints = [ (x, y, z) | x <- [from xr..to xr]
                                , y <- [from yr..to yr]
                                , z <- [from zr..to zr] ]
    let reverseSteps = reverse rebootSteps
    let numberOn = length $ filter (==On) $ map (firstInside reverseSteps) secPoints
    putStrLn $ show numberOn
