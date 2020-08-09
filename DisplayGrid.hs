module DisplayGrid where

import Type

{-
Desideriamo che [[B,O,O],[O,X,O],[X,X,X]] venga mostrata come:
   |   |
   | O | O
   |   |
-----------
   |   |
 O | X | O
   |   |
-----------
   |   |
 X | X | X
   |   |
-}

showPlayer :: Player -> [String]
showPlayer p = case p of
    O -> ["   ", " O ", "   "]
    B ->  ["   ", "   ", "   "]
    X -> ["   ", " X ", "   "]
{-
Alla fine, ognuna di questa lista di stringhe verrà
stampate un verticale
-}

-- Inserisce un valore ("|") tra i valori di una lista
interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [x] = [x]
interleave a (x:xs) = x : a : interleave a xs

{-
Scrive un'intera riga. Del tipo con [O,X,B]:
["   |   |   ",
" O | X |   ",
"   |   |   "]
-}
showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"
{-
zipWith (++) ["a", "b", "c"] ["d", "e", "f"] = ["ad","be","cf"]
map showPlayer [O,X,B] = [["   "," O ","   "],["   "," X ","   "],["   ","   ","   "]]
interleave bar . map showPlayer [O,X,B] = S = [["   "," O ","   "],
                                              ["|","|","|"],
                                              ["   "," X ","   "],
                                              ["|","|","|"],
                                              ["   ","   ","   "]]
beside S, partendo dal fondo, prende le ultime 2 liste di stringhe e le unisce
attraverso la funzione "zipWith (++)". Perciò al primo step con
["|","|","|"] e ["   ","   ","   "] avremo
["|   ","|   ","!   "]. Al secondo ,con ["   "," X ","   "] avremo
["   |   "," X |   ","   |   "]
-}

-- Data una gliglia, la stampa
putGrid :: Grid -> IO()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-'] -- ["-----------"]

{-
map showRow [[B,O,O],[O,X,O],[X,X,X]] = [["   |   |   ",
                                          "   | O | O ",
                                          "   |   |   "],
                                          ["   |   |   ",
                                          " O | X | O ",
                                          "   |   |   "],
                                          ["   |   |   ",
                                          " X | X | X ",
                                          "   |   |   "]]
interleave bar . map shoRow [[B,O,O],[O,X,O],[X,X,X]] = [["   |   |   ",
                                                          "   | O | O ",
                                                          "   |   |   "],
                                                         ["-----------"],
                                                         ["   |   |   ",
                                                          " O | X | O ",
                                                          "   |   |   "],
                                                         ["-----------"],
                                                         ["   |   |   ",
                                                          " X | X | X ",
                                                          "   |   |   "]]
concat interleave bar . map shoRow [[B,O,O],[O,X,O],[X,X,X]] = ["   |   |   ",
                                                                "   | O | O ",
                                                                "   |   |   ",
                                                                "-----------",
                                                                "   |   |   ",
                                                                " O | X | O ",
                                                                "   |   |   ",
                                                                "-----------",
                                                                "   |   |   ",
                                                                " X | X | X ",
                                                                "   |   |   "]
-}