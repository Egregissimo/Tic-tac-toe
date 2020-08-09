module GridUtilities where

import Data.List
import Type

-- Indica qual è il prossimo giocatore
next :: Player -> Player
next p = case p of
    O -> X
    B -> B
    X -> O

empty :: Grid
empty = replicate size (replicate size B)

{-
Indica se una griglia non possiede B
Data la matrice di valori, li "srotola" e
valuta se tutti i suoi valori sono diversi da B
-}
full :: Grid -> Bool
full = all (/= B) . concat

{-
In base ai valori nella griglia è possibile
capire di chi è il turno.
si presuppone che O inizi per primo
-}
turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

-- Restituisce la digonale di una griglia
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size-1]]

{-
Data una griglia ed un giocatore p, indica
se quel giocatore p ha vinto.
Concatena tutte le righe, le colonne e le
diagonali ed infine verifica se in una di queste
ha tutti e 3 i valori sono del giocatore p.
-}
wins :: Player -> Grid -> Bool
wins p g =any line (rows ++ cols ++ diags)
    where
        line = all (== p)
        rows = g
        cols = transpose g
        diags = [diag g, diag (map reverse g)]
{-
La diagonale inversa di g di ottine invertendo
i valori di ogni singola riga, grazie alla funzione
map che valuta i valori delle singole righe.
-}

-- Indica se uno dei due giocatori ha vinto
won :: Grid -> Bool
won g = wins O g || wins X g