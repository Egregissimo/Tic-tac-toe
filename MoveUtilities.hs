module MoveUtilities where

import Type
import GridUtilities

-- Indica se una mossa è valida
valid :: Grid -> Int -> Bool
valid g i = i >= 0 && i <= size^2 && concat g !! i == B

-- Data una lista di valori, li divide in gruppi di n elementi
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

{-
Data una griglia, una mossa ed un giocatore, viene restituita
la nuova griglia.
Se la mossa non è valida viene restituita una lista vuota, altrimenti
una lista con un singolo elemento (la nuova griglia). Non viene usato
Maybe perché usare le liste sarà utile più avanti nel gestire
l'algoritmo min-max
-}
move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)
-- (xs,B:ys) è l'unico pattern possibile dato che valid g i = true

-- FUNCTION FOR MINIMAX ALGORITHM

depth :: Int
depth = 9

{-
Grazie alla lazy evaluation, prune non taglia un albero già
completamente creato, semplicemente evita di creare i rami
sotto la profonditò indicata
-}
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- Crea tutte le possibili mosse che può fare un giocatore data una griglia 
moves :: Grid -> Player -> [Grid]
moves g p | won g     = [] -- foglia perché qualcuno ha vinto
          | full g    = [] -- foglia cin una situazione di parità
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

