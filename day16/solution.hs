import Data.Maybe

type Version = Int
type TypeId = Int

data Packet = Literal Header Int Int
            | Operator Header [Packet] deriving (Show)

data Header = VT Int Int deriving Show

data Bit = Zero | One deriving (Show)
type BitString = [Bit]

bit Zero = 0
bit One = 1

bits :: BitString -> Int
bits = foldl (\v b -> 2 * v + (bit b)) 0

packetLength :: Packet -> Int
packetLength (Literal _ nGroups _) = 3 + 3 + nGroups * 5
packetLength (Operator _ ps) = sum $ map packetLength ps

data ParseResult a = PR a BitString | EndOfInput deriving (Show)
type Parser a = BitString -> ParseResult a

parsePacket :: Parser Packet
parsePacket [] = EndOfInput
parsePacket bs
    | typeId == 4 = parseLiteral header bs
    | otherwise   = parseOperator header bs
    where (PR header hRemainder) = parseHeader bs
          (VT _ typeId) = header

parseHeader :: Parser Header  
parseHeader [] = EndOfInput
parseHeader bs = PR (VT v t) tr
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