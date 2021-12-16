import Data.Maybe

type Version = Int

data Packet = Literal Version Int | Operator Version [Packet] deriving (Show)

version (Literal v _) = v
version (Operator v _) = v

literal (Literal _ v) = Just v
literal (Operator _ _) = Nothing

subpackets (Literal _ _) = Nothing
subpackets (Operator _ sp) = Just sp

data Bit = Zero | One

bitToInt Zero = 0
bitToInt One = 1

bitStringToInt :: [Bit] -> Int
bitStringToInt = foldl (\v b -> 2 * v + (bitToInt b)) 0

--parse :: [Bit] -> (Maybe Packet, [Bit])
--parse [] = (Nothing, [])
--parse (v4:v2:v1:bs) = 