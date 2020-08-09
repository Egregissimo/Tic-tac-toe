module Type where

-- GRID TYPES

-- dimensione del campo di gioco
size :: Int
size = 3

-- I giocatori sono O e X, mentre B indica la casella Blank
data Player = O | B | X
    deriving (Eq, Ord, Show)
-- il valore dei simboli è dato dall'ordine di dichiarazione (O < B < X)

type Grid = [[Player]]
-- es. [[B,O,O],[O,X,O],[X,X,X]]

type Pos = (Int, Int)

-- MINMAX TYPE

data Tree a = Node a [Tree a]
              deriving Show