import Data.List (intercalate)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (Maybe(Just))
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
    show (Literal (Hdr version _) _ value)       = (show version) ++ "=" ++ (show value)
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
        ('9', "1011"),
        ('A', "1000"),
        ('B', "1011"),
        ('C', "1110"),
        ('D', "1101"),
        ('E', "1110"),
        ('F', "1111")
    ]

hexToBits :: String -> BitString
hexToBits [] = []
hexToBits (h:hs) = (bitsFromString firstBits) ++ moreBits
    where (Just firstBits) = Map.lookup h hexMap
          moreBits = hexToBits hs

-------------
-- Parsers --

data ParseResult a = PR a BitString | EndOfInput deriving Show
type Parser a = BitString -> ParseResult a

parsePacket :: Parser Packet
parsePacket [] = EndOfInput
parsePacket bs
    | typeId == 4 = parseLiteral header bs
    | otherwise   = parseOperator header bs
    where (PR header hRemainder) = parseHeader bs
          (Hdr _ typeId) = header

parseHeader :: Parser Header  
parseHeader [] = EndOfInput
parseHeader bs = PR (Hdr v t) tr
    where (PR v vr) = parseVersion bs
          (PR t tr) = parseTypeId vr

parseLiteral :: Header -> Parser Packet
parseLiteral h bs = PR packet gRemainder
    where (PR header hRemainder) = parseHeader bs
          (PR groups gRemainder) = parseGroups hRemainder
          value = bits $ concat groups
          packet = Literal h (length groups) value

parseOperator :: Header -> Parser Packet
parseOperator h (Zero:bs) = PR packet spRemainder
    where PR spLength lRemainder = parseInt 15 bs
          PR subpackets spRemainder = parseSubpacketsByLength spLength lRemainder
          packet = Operator h subpackets
parseOperator h (One:bs) = PR packet spRemainder
    where PR spCount lRemainder = parseInt 11 bs
          PR subpackets spRemainder = parseSubpacketsByCount spCount lRemainder
          packet = Operator h subpackets

parseSubpacketsByCount :: Int -> Parser [Packet]
parseSubpacketsByCount 0 bs = PR [] bs
parseSubpacketsByCount n bs = PR (first:more) mspRemainder
    where PR first fRemainder = parsePacket bs
          PR more mspRemainder = parseSubpacketsByCount (n-1) fRemainder

parseSubpacketsByLength :: Int -> Parser [Packet]
parseSubpacketsByLength 0 bs = PR [] bs
parseSubpacketsByLength len bs = PR (first:more) mspRemainder
    where PR first fRemainder = parsePacket bs
          remainingLength = len - (packetLength first)
          PR more mspRemainder = parseSubpacketsByLength remainingLength fRemainder

parseInt :: Int -> Parser Int
parseInt n bs = PR (bits (take n bs)) (drop n bs)
parseVersion = parseInt 3
parseTypeId = parseInt 3

-- Remove all the leading marks from groups to get the original bit string
-- representing the literal value.
parseGroups :: Parser [BitString]
parseGroups (Zero:bs) = PR [first] fRemainder
    where (PR first fRemainder) = parseGroup bs
parseGroups (One:bs) = PR (first:more) mRemainder
    where (PR first fRemainder) = parseGroup bs
          (PR more mRemainder) = parseGroups fRemainder

parseGroup :: Parser BitString
parseGroup bs = PR (take 4 bs) (drop 4 bs)

--------------
-- Examples --

doExample :: String -> Packet -> IO ()
doExample h expected = do
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Actual  : " ++ show actual
    putStrLn $ "Remainder: " ++ bitsToString remainder
    where bs = hexToBits h
          (PR actual remainder) = parsePacket bs

example1 = doExample "D2FE28" (Literal (Hdr 6 4) 3 2021)

example2 = doExample "38006F45291200" (Operator (Hdr 1 6) [
        (Literal (Hdr 0 4) 1 10),
        (Literal (Hdr 0 4) 2 20)
    ])

example3 = doExample "EE00D40C823060" (Operator (Hdr 7 3) [
        (Literal (Hdr 0 4) 1 1),
        (Literal (Hdr 0 4) 1 2),
        (Literal (Hdr 0 4) 1 3)
    ])

example4 = doExample "8A004A801A8002F478" (Operator (Hdr 4 0) [
        (Operator (Hdr 1 0) [
            (Operator (Hdr 5 0) [
                (Operator (Hdr 6 0) [
                ])
            ])
        ])
    ])
