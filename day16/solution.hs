import Data.List (intercalate)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (Maybe(Just))
import Debug.Trace
import Numeric

-----------------------------------------
-- Definition of Packet and its pieces --

type Version = Int
type TypeId = Int

data Header = Hdr Version TypeId

instance Show Header where
    show (Hdr v t) = (show v) ++ (show t)

data Packet = Literal Header Int Int
            | Operator Header [Packet]

instance Show Packet where
    show (Literal (Hdr version _) numGroups value)       = (show version) ++ "=" ++ (show value) ++ "(" ++ show numGroups ++ ")"
    show (Operator (Hdr version _) subpackets) = (show version) ++ "{" ++ (intercalate ", " $ map show subpackets) ++ "}"

packetLength :: Packet -> Int
packetLength (Literal _ nGroups _) = 3 + 3 + nGroups * 5
packetLength (Operator _ ps) = sum $ map packetLength ps

-----------------------
-- Dealing with bits --

data Bit = Zero | One deriving Show

type BitString = [Bit]

bit Zero = 0
bit One = 1

bits :: BitString -> Int
bits = foldl (\v b -> 2 * v + (bit b)) 0

bitsFromString [] = []
bitsFromString ('0':bs) = Zero : (bitsFromString bs)
bitsFromString ('1':bs) = One : (bitsFromString bs)

bitsToString [] = []
bitsToString (Zero:bs) = '0' : (bitsToString bs)
bitsToString (One :bs) = '1' : (bitsToString bs)

hexMap = Map.fromList [
        ('0', "0000"),
        ('1', "0001"),
        ('2', "0010"),
        ('3', "0011"),
        ('4', "0100"),
        ('5', "0101"),
        ('6', "0110"),
        ('7', "0111"),
        ('8', "1000"),
        ('9', "1001"),
        ('A', "1010"),
        ('B', "1011"),
        ('C', "1100"),
        ('D', "1101"),
        ('E', "1110"),
        ('F', "1111")
    ]

hexToBits :: String -> BitString
hexToBits [] = []
hexToBits "\n" = []
hexToBits (h:hs) = (bitsFromString firstBits) ++ moreBits
    where (Just firstBits) = Map.lookup h hexMap
          moreBits = hexToBits hs

-------------
-- Parsers --

data ParseResult a = PR a BitString | EndOfInput deriving Show
type Parser a = BitString -> ParseResult a

ptrace f bs = trace ("TRACE " ++ f ++ " [" ++ bitsToString bs ++ "]")

parsePacket :: Parser Packet
parsePacket [] = error "Expected packet"
parsePacket bs
    | typeId == 4 = parseLiteral header hRemainder
    | otherwise   = parseOperator header hRemainder
    where PR header hRemainder = --ptrace "packet" bs $
                                 parseHeader bs
          Hdr _ typeId = header

parseHeader :: Parser Header  
parseHeader [] = error "Expected header"
parseHeader bs = PR (Hdr v t) tr
    where PR v vr = parseVersion bs
          PR t tr = parseTypeId vr

parseLiteral :: Header -> Parser Packet
parseLiteral _ [] = error "Expected literal packet"
parseLiteral h bs = PR packet gRemainder
    where PR groups gRemainder = parseGroups bs
          value = bits $ concat groups
          packet = Literal h (length groups) value

parseOperator :: Header -> Parser Packet
parseOperator _ [] = error "Expected operator packet"
parseOperator h (Zero:bs) = PR packet spRemainder
    where PR spLength lRemainder = parseInt 15 bs
          PR subpackets spRemainder = --trace ("LENGTH=" ++ bitsToString (take 15 bs)) $
                                      parseSubpacketsByLength spLength lRemainder
          packet = Operator h subpackets
parseOperator h (One:bs) = PR packet spRemainder
    where PR spCount lRemainder = parseInt 11 bs
          PR subpackets spRemainder = parseSubpacketsByCount spCount lRemainder
          packet = Operator h subpackets

parseSubpacketsByCount :: Int -> Parser [Packet]
parseSubpacketsByCount n [] = error $ "Expected " ++ show n ++ " subpackets"
parseSubpacketsByCount 0 bs = PR [] bs
parseSubpacketsByCount n bs = PR (first:more) mspRemainder
    where PR first fRemainder = parsePacket bs
          PR more mspRemainder = parseSubpacketsByCount (n-1) fRemainder

parseSubpacketsByLength :: Int -> Parser [Packet]
parseSubpacketsByLength n [] = error $ "Expected " ++ show n ++ " bits"
parseSubpacketsByLength 0 bs = PR [] bs
parseSubpacketsByLength len bs = PR (first:more) mspRemainder
    where PR first fRemainder = --ptrace ("parseSubpacketsByLength" ++ show len) bs $
                                parsePacket bs
          remainingLength = len - (packetLength first)
          PR more mspRemainder = --trace ("First=[" ++ show first ++ "], |First|=" ++ show (packetLength first) ++ ", Remaining " ++ show remainingLength ++ " [" ++ bitsToString fRemainder ++ "]") $
                                 parseSubpacketsByLength remainingLength fRemainder

parseInt :: Int -> Parser Int
parseInt n [] = error $ "Expected " ++ show n ++ " bits forming an int."
parseInt n bs = PR (bits (take n bs)) (drop n bs)
parseVersion = parseInt 3
parseTypeId = parseInt 3

-- Remove all the leading marks from groups to get the original bit string
-- representing the literal value.
parseGroups :: Parser [BitString]
parseGroups [] = error "Expected groups"
parseGroups (Zero:bs) = PR [first] fRemainder
    where PR first fRemainder = parseGroup bs
parseGroups (One:bs) = PR (first:more) mRemainder
    where PR first fRemainder = parseGroup bs
          PR more mRemainder = parseGroups fRemainder

parseGroup :: Parser BitString
parseGroup [] = error "Expected group"
parseGroup bs = PR (take 4 bs) (drop 4 bs)

--------------
-- Examples --

doExample :: String -> Packet -> IO ()
doExample h expected = do
    putStrLn $ "Expected: " ++ show expected ++ " SUM=" ++ (show $ versionSum expected)
    putStrLn $ "Actual  : " ++ show actual ++ " SUM=" ++ (show $ versionSum actual)
    putStrLn $ "Remainder: " ++ bitsToString remainder
    where bs = hexToBits h
          (PR actual remainder) = parsePacket bs

example1 = doExample "D2FE28" (Literal (Hdr 6 4) 3 2021)

example2hex = "38006F45291200"
example2bits = hexToBits example2hex
example2 = doExample example2hex (Operator (Hdr 1 6) [
        (Literal (Hdr 0 4) 1 10),
        (Literal (Hdr 0 4) 2 20)
    ])

example3hex = "EE00D40C823060"
example3bits = hexToBits example3hex
example3 = doExample example3hex (Operator (Hdr 7 3) [
        (Literal (Hdr 0 4) 1 1),
        (Literal (Hdr 0 4) 1 2),
        (Literal (Hdr 0 4) 1 3)
    ])

example4hex = "8A004A801A8002F478"
example4bits = hexToBits example4hex
example4 = doExample example4hex (Operator (Hdr 4 0) [
        (Operator (Hdr 1 0) [
            (Operator (Hdr 5 0) [
                (Literal (Hdr 6 0) 0 0)
            ])
        ])
    ])

example5hex = "620080001611562C8802118E34"
example5bits = hexToBits example4hex
example5 = doExample example5hex $
    Operator (Hdr 3 0) [
        Operator (Hdr 0 0) [Literal (Hdr 0 0) 0 0, Literal (Hdr 5 0) 0 0],
        Operator (Hdr 1 0) [Literal (Hdr 0 0) 0 0, Literal (Hdr 3 0) 0 0]
    ]

------------------------------
-- Actual requested answers --

versionSum (Literal (Hdr v _) _ _) = v
versionSum (Operator (Hdr v _) subpackets) = v + (sum $ map versionSum subpackets)

part1 filename = do
    line <- readFile filename
    let bs = hexToBits line
    let PR packet _ = parsePacket bs
    putStrLn $ filename ++ (show $ versionSum packet)

main = do
    part1 "full.txt"
