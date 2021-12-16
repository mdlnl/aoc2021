import Data.Maybe

type Version = Int
type TypeId = Int

data Packet = Literal Version TypeId Int Int
            | Operator Version TypeId [Packet] deriving (Show)

data Bit = Zero | One deriving (Show)
type BitString = [Bit]

bit Zero = 0
bit One = 1

bits :: BitString -> Int
bits = foldl (\v b -> 2 * v + (bit b)) 0

packetLength :: Packet -> Int
packetLength (Literal _ _ nGroups _) = 3 + 3 + nGroups * 5
packetLength (Operator _ _ ps) = sum $ map packetLength ps

data ParseResult a = PR a BitString | EndOfInput deriving (Show)
type Parser a = BitString -> Parse a

parsePacket :: Parser Packet
parsePacket [] = EndOfInput
parsePacket bs
    | typeId == 4 = parseLiteral version typeId tRemainder
    | otherwise   = parseOperator version typeId tRemainder
    where PR version vRemainder = parseVersion bs
          PR typeId tRemainder = parseTypeId vRemainder

parseLiteral :: Parser Packet
parseLiteral v t bs = PR packet remainder
    where PR groupBits remainder = parseGroups bs
          packet = Literal v t ((length groupBits) / 4) (bits groupBits)

parseOperator :: Parser Packet
parseOperator v t (Zero:bs) = PR packet spRemainder
    where PR spLength lRemainder = parseInt 15 bs
          PR subpackets spRemainder = parseSubacketsByLength spLength lRemainder
          packet = Operator v t subpackets

parseSubpacketsByLength :: Parser [Packet]
parseSubpacketsByLength 0 bs = PR [] bs
parseSubpacketsByLength len bs = PR (first:more) mspRemainder
    where PR first fRemainder = parsePacket bs
          remainingLength = len - (packetLength first)
          PR more mspRemainder = parseSubacketsByLength remainingLength fRemainder

parseInt :: Parser Int
parseInt n bs = PR (bits (take n bs)) (drop n bs)
parseVersion = parseInt 3
parseTypeId = parseInt 3

-- Remove all the leading marks from groups to get the original bit string
-- representing the literal value.
parseGroups :: Parser BitString
parseGroups (Zero:g8:g4:g2:g1:bs) = PGR [g8,g4,g2,g1] bs
parseGroups (One:g8:g4:g2:g1:bs) = PGR (g8:g4:g2:g1:gs) remainder
    where (PGR gs remainder) = parseGroups bs
