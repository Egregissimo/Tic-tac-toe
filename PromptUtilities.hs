module PromptUtilities where

import Data.Char
import Type

-- Per ottenere l'input di un altro giocatore
getNat :: String -> IO Int
getNat input = do
    putStr input
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else do
        putStrLn "ERROR: Invalid number"
        getNat input

-- Pulisce il terminale
cls :: IO()
cls = putStr "\ESC[2J"

-- Muove il cursore nella posizione indicata
goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Indica qual'è il prossimo giocatore che deve muovere
prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "