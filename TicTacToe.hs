module TicTacToe where

import System.IO
import Type
import GridUtilities
import DisplayGrid
import PromptUtilities
import MoveUtilities
import Minimax
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- HUMAN VS HUMAN

-- Data una griglia ed un giocatore, esegue il gioco
run :: Grid -> Player -> IO ()
run g p = do
    cls
    goto (1,1)
    putGrid g
    run' g p

-- Funzione ausiliaria di run
run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    []   -> do putStrLn "ERROR: Invalid move"
                               run' g p
                    [g'] -> run g' (next p)

{-
le mosse sono date attraverso un intero:
0|1|2
-----
3|4|5
-----
6|7|8
-}

-- Esecuzione tra due giocatori
tictactoe :: IO ()
tictactoe = do
    run empty O

-- HUMAN VS COMPUTER

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    play empty O 1

play :: Grid -> Player -> Int -> IO ()
play g p n = do
    cls
    goto (1,1)
    putGrid g
    play' g p n

{-
C'è la valutazione strict perché altrimenti il gioco
proseguirebbe senza che venga calcolata la prossima
mossa del pc
-}
play' :: Grid -> Player -> Int -> IO ()
play' g p n
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play' g p n
                      [g'] -> play g' (next p) n
   | p == X   = do
       putStr "Player X is thinking... "
       start <- getCPUTime
       (play $! (bestmove g p)) (next p) (n+1)
       end <- getCPUTime
       let diff = (fromIntegral (end - start)) / (10^12)
       printf "Computation time move %d: %0.3f sec\n" n (diff :: Double)
       