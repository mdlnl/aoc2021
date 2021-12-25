import Data.Map (Map, fromList, insert)
import qualified Data.Map as Map
import Data.Maybe
import Split

data Variable = W | X | Y | Z deriving (Eq, Ord)

data Arg = Var Variable | Lit Integer | Empty

data Instruction = Inp Variable Arg
                 | Add Variable Arg
                 | Mul Variable Arg
                 | Div Variable Arg
                 | Mod Variable Arg
                 | Eql Variable Arg

op (Add _ _) = (+)
op (Mul _ _) = (*)
op (Div _ _) = div
op (Mod _ _) = mod
op (Eql _ _) = \a b -> if a == b then 1 else 0

type Memory = Map Variable Integer

initialMemory = fromList [(W, 0), (X, 0), (Y, 0), (Z, 0)]

get :: Memory -> Arg -> Integer
get m (Var v) = fromJust $ Map.lookup v m
get _ (Lit x) = x
