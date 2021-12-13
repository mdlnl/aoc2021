module LinkedGrid where

-- DLG rowsAboveReversed rowsAtOrBelow columnIndex
data DoublyLinkedGrid a = DLG [a] [a] Int

--instance Show a => Show (DoublyLinkedGrid a) where
  --show (DLG aboves atOrBelows columnIndex) = showDlgColMarker

fromListOfLists outerList = DLG [] outerList 0

rows (DLG aboves atOrBelows _) = length aboves + length atOrBelows

columns (DLG _ (at:_) _) = length at

up (DLG [] _ _) = error "Already at the top."
up (DLG (above:aboves) atOrBelows columnIndex) = DLG aboves (above:atOrBelows) columnIndex

down (DLG _ [] _) = error "Already at the bottom."
down (DLG aboves (at:belows) columnIndex) = DLG (at:aboves) belows columnIndex

left (DLG _ _ 0) = error "Already at the left edge."
left (DLG aboves atOrBelows columnIndex) = DLG aboves atOrBelows (columnIndex - 1)

right (DLG _ [] _) = error "Fell off the world."
right (DLG aboves (at:belows) columnIndex)
    | length at > columnIndex + 1 = DLG aboves (at:belows) (columnIndex + 1)
    | otherwise                   = error "Already at the right edge."
