import Split
import Testing

data Range = Range { from::Int, to::Int } deriving Eq
instance Show Range where show (Range f t) = show f ++ ".." ++ show t

data Value = On | Off deriving (Eq, Show)

data RebootStep = RebootStep { value::Value, xrange::Range, yrange::Range, zrange::Range } deriving Eq
instance Show RebootStep where show (RebootStep v xr yr zr) = show v ++ "x=" ++ show xr
                                                                     ++ "y=" ++ show yr
                                                                     ++ "z=" ++ show zr

parseRange fromDotDotTo = Range { from = read fromStr :: Int, to = read toStr :: Int }
    where [fromStr, toStr] = split ".." fromDotDotTo

testParseRange = doTests action [ TC "1..2"         $ Range 1 2
                                , TC "-1234..-7890" $ Range (-1234) (-7890) ]
    where action (TC i e) = expect e $ parseRange i

parseValue "on" = On
parseValue "off" = Off

parseRebootStep line = RebootStep { value = parseValue value
                                  , xrange = parseRange xrange
                                  , yrange = parseRange yrange
                                  , zrange = parseRange zrange }
    where [value, ranges] = split " " line
          [xrange, yrange, zrange] = split "," ranges

parseInput input = map parseRebootStep $ nlsplit input

parseFile filename = do
    input <- readFile filename
    return $ parseInput input
