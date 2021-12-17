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
parseSubpacketsByLength 0 bs = PR [] bs
parseSubpacketsByLength len bs
    | length bs < len = error $ "Expected " ++ show len ++ " bits for subpackets, but only got " ++ (show $ length bs)
    | otherwise = PR (first:more) mspRemainder
    where PR first fRemainder = --ptrace ("parseSubpacketsByLength" ++ show len) bs $
                                parsePacket bs
          remainingLength = len - (packetLength first)
          PR more mspRemainder = --trace ("First=[" ++ show first ++ "], |First|=" ++ show (packetLength first) ++ ", Remaining " ++ show remainingLength ++ " [" ++ bitsToString fRemainder ++ "]") $
                                 parseSubpacketsByLength remainingLength fRemainder

parseInt :: Int -> Parser Int
parseInt n bs
    | length bs < n = error $ "Expected " ++ show n ++ " bits forming an int, but got " ++ (show $ length bs)
    | otherwise     = PR (bits (take n bs)) (drop n bs)
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
parseGroup bs
    | length bs < 4 = error "Expected 4 bits for group"
    | otherwise     = PR (take 4 bs) (drop 4 bs)

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
example2 = doExample example2hex $
    Operator (Hdr 1 6) [
        Literal (Hdr 0 4) 1 10,
        Literal (Hdr 0 4) 2 20
    ]

example3hex = "EE00D40C823060"
example3bits = hexToBits example3hex
example3 = doExample example3hex $
    Operator (Hdr 7 3) [
        Literal (Hdr 0 4) 1 1,
        Literal (Hdr 0 4) 1 2,
        Literal (Hdr 0 4) 1 3
    ]

example4hex = "8A004A801A8002F478"
example4bits = hexToBits example4hex
example4 = doExample example4hex $
    Operator (Hdr 4 0) [
        Operator (Hdr 1 0) [
            Operator (Hdr 5 0) [
                (Literal (Hdr 6 0) 0 0)
            ]
        ]
    ]

example5hex = "620080001611562C8802118E34"
example5bits = hexToBits example4hex
example5 = doExample example5hex $
    Operator (Hdr 3 0) [
        Operator (Hdr 0 0) [Literal (Hdr 0 0) 0 0, Literal (Hdr 5 0) 0 0],
        Operator (Hdr 1 0) [Literal (Hdr 0 0) 0 0, Literal (Hdr 3 0) 0 0]
    ]

example6hex = "C0015000016115A2E0802F182340"
example6bits = hexToBits example6hex
example6 = doExample example6hex $
    Operator (Hdr 3 0) [
        Operator (Hdr 0 0) [Literal (Hdr 0 0) 0 0, Literal (Hdr 5 0) 0 0],
        Operator (Hdr 1 0) [Literal (Hdr 0 0) 0 0, Literal (Hdr 3 0) 0 0]
    ]

------------------------------
-- Actual requested answers --

versionSum (Literal (Hdr v _) _ _) = v
versionSum (Operator (Hdr v _) subpackets) = v + (sum $ map versionSum subpackets)

fullHex = "020D708041258C0B4C683E61F674A1401595CC3DE669AC4FB7BEFEE840182CDF033401296F44367F938371802D2CC9801A980021304609C431007239C2C860400F7C36B005E446A44662A2805925FF96CBCE0033C5736D13D9CFCDC001C89BF57505799C0D1802D2639801A900021105A3A43C1007A1EC368A72D86130057401782F25B9054B94B003013EDF34133218A00D4A6F1985624B331FE359C354F7EB64A8524027D4DEB785CA00D540010D8E9132270803F1CA1D416200FDAC01697DCEB43D9DC5F6B7239CCA7557200986C013912598FF0BE4DFCC012C0091E7EFFA6E44123CE74624FBA01001328C01C8FF06E0A9803D1FA3343E3007A1641684C600B47DE009024ED7DD9564ED7DD940C017A00AF26654F76B5C62C65295B1B4ED8C1804DD979E2B13A97029CFCB3F1F96F28CE43318560F8400E2CAA5D80270FA1C90099D3D41BE00DD00010B893132108002131662342D91AFCA6330001073EA2E0054BC098804B5C00CC667B79727FF646267FA9E3971C96E71E8C00D911A9C738EC401A6CBEA33BC09B8015697BB7CD746E4A9FD4BB5613004BC01598EEE96EF755149B9A049D80480230C0041E514A51467D226E692801F049F73287F7AC29CB453E4B1FDE1F624100203368B3670200C46E93D13CAD11A6673B63A42600C00021119E304271006A30C3B844200E45F8A306C8037C9CA6FF850B004A459672B5C4E66A80090CC4F31E1D80193E60068801EC056498012804C58011BEC0414A00EF46005880162006800A3460073007B620070801E801073002B2C0055CEE9BC801DC9F5B913587D2C90600E4D93CE1A4DB51007E7399B066802339EEC65F519CF7632FAB900A45398C4A45B401AB8803506A2E4300004262AC13866401434D984CA4490ACA81CC0FB008B93764F9A8AE4F7ABED6B293330D46B7969998021C9EEF67C97BAC122822017C1C9FA0745B930D9C480"
fullBits = hexToBits fullHex
PR fullPacket _ = parsePacket fullBits

annaHex = "E0529D18025800ABCA6996534CB22E4C00FB48E233BAEC947A8AA010CE1249DB51A02CC7DB67EF33D4002AE6ACDC40101CF0449AE4D9E4C071802D400F84BD21CAF3C8F2C35295EF3E0A600848F77893360066C200F476841040401C88908A19B001FD35CCF0B40012992AC81E3B980553659366736653A931018027C87332011E2771FFC3CEEC0630A80126007B0152E2005280186004101060C03C0200DA66006B8018200538012C01F3300660401433801A6007380132DD993100A4DC01AB0803B1FE2343500042E24C338B33F5852C3E002749803B0422EC782004221A41A8CE600EC2F8F11FD0037196CF19A67AA926892D2C643675A0C013C00CC0401F82F1BA168803510E3942E969C389C40193CFD27C32E005F271CE4B95906C151003A7BD229300362D1802727056C00556769101921F200AC74015960E97EC3F2D03C2430046C0119A3E9A3F95FD3AFE40132CEC52F4017995D9993A90060729EFCA52D3168021223F2236600ECC874E10CC1F9802F3A71C00964EC46E6580402291FE59E0FCF2B4EC31C9C7A6860094B2C4D2E880592F1AD7782992D204A82C954EA5A52E8030064D02A6C1E4EA852FE83D49CB4AE4020CD80272D3B4AA552D3B4AA5B356F77BF1630056C0119FF16C5192901CEDFB77A200E9E65EAC01693C0BCA76FEBE73487CC64DEC804659274A00CDC401F8B51CE3F8803B05217C2E40041A72E2516A663F119AC72250A00F44A98893C453005E57415A00BCD5F1DD66F3448D2600AC66F005246500C9194039C01986B317CDB10890C94BF68E6DF950C0802B09496E8A3600BCB15CA44425279539B089EB7774DDA33642012DA6B1E15B005C0010C8C917A2B880391160944D30074401D845172180803D1AA3045F00042630C5B866200CC2A9A5091C43BBD964D7F5D8914B46F040"
annaBits = hexToBits fullHex
PR annaPacket _ = parsePacket annaBits

part1 filename = do
    line <- readFile filename
    let bs = hexToBits line
    let PR packet _ = parsePacket bs
    putStrLn $ filename ++ (show $ versionSum packet)

main = do
    --part1 "full.txt"
    putStrLn $ show annaPacket
