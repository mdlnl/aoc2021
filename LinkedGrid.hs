module LinkedGrid where

import Split

-- LG rowsAboveReversed rowsAtOrBelow columnIndex
data LinkedGrid a = LG [a] [a] Int deriving Show

--instance Show a => Show (LinkedGrid a) where
  --show (LG aboves atOrBelows columnIndex) = showDlgColMarker

fromListOfLists outerList = LG [] outerList 0

fromString rowDelim colDelim string = 
    fromListOfLists [ split colDelim row | row <- split rowDelim string ]

map f (LG aboves atOrBelows col) =
        LG (pmap rowf aboves) (pmap rowf atOrBelows) col
    where pmap = Prelude.map
          rowf = pmap f

rows (LG aboves atOrBelows _) = length aboves + length atOrBelows

columns (LG _ (at:_) _) = length at

up (LG [] _ _) = error "Already at the top."
up (LG (above:aboves) atOrBelows columnIndex) = LG aboves (above:atOrBelows) columnIndex

down (LG _ [] _) = error "Already at the bottom."
down (LG aboves (at:belows) columnIndex) = LG (at:aboves) belows columnIndex

left (LG _ _ 0) = error "Already at the left edge."
left (LG aboves atOrBelows columnIndex) = LG aboves atOrBelows (columnIndex - 1)

right (LG _ [] _) = error "Fell off the world."
right (LG aboves (at:belows) columnIndex)
    | length at > columnIndex + 1 = LG aboves (at:belows) (columnIndex + 1)
    | otherwise                   = error "Already at the right edge."
