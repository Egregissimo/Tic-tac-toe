module Minimax where

import Type
import GridUtilities
import MoveUtilities

-- Costruisce l'albero con tutte le possibili mosse partendo da una griglia ed un giocatore
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

{-
La mossa migliore dalla radice r è il sottonodo con
la stessa etichetta di r
-}
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimaxPruning tree
                  
-- Minimax
{-
Algoritmo che suggerisce al computer che mossa fare.
Dato che il pc gioca con gli X, gli scenari dove vince
lui valgono 1, in parità 0 e dove perde -1 (qui si vede
perché l'ordine di dichiarazione di O, B e X).
Dunque si cercherà il minimo quando muove O e il massimo
quando muove X.
Ora il tipo dell'algero è una tupla (gliglia, etichetta).
L'etichettà è data dal minimo o massimo delle etichettè
dei nodi figli.
-}
minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts) 
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']

-- Versione di MinMax con alpha-beta pruning (più veloce)

minimaxPruning :: Tree Grid -> Tree (Grid,Player)
minimaxPruning (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimaxPruning (Node g ts@(x:xs)) 
   | turn g == O = Node (g, minimum ps') ts'
   | turn g == X = Node (g, maximum ps'') ts''
                   where
                      t@(Node (_, label) _) = minimaxPruning x
                      ts' = t : betaPruning xs label
                      ps'  = [p | Node (_,p) _ <- ts']
                      ts'' = t : alphaPruning xs label
                      ps'' = [p | Node (_,p) _ <- ts'']

betaPruning :: [Tree Grid] -> Player -> [Tree (Grid,Player)]
betaPruning [] _ = []
betaPruning (t:ts) p = let newNode@(Node (_, p') _) = minimaxPruning t
                              in if p' < p then
                                 newNode : betaPruning ts p'
                                 else
                                    betaPruning ts p

alphaPruning :: [Tree Grid] -> Player -> [Tree (Grid,Player)]
alphaPruning [] _ = []
alphaPruning (t:ts) p = let newNode@(Node (_, p') _) = minimaxPruning t
                              in if p' > p then
                                 newNode : alphaPruning ts p'
                                 else
                                    alphaPruning ts p