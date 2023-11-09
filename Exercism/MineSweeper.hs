module Minesweeper (annotate) where

import Data.Char (isSpace)

annotate :: [String] -> [String]
annotate board
  | all (all isSpace) board = board
  | all (all (\x -> x == '*')) board = board
  | otherwise = []  --createMinesBoard board

-- createMinesBoard str = [] 
-- ["-*-*-" "--*--" "--*--" "-----"]
-- Adjacent row column -> row-1 column row+1 column row column-1 row+1 column
--                        row-1 column-1 row-1 column+1 row+1 column-1 row+1 column+1
-- #rows = length board
-- #columns = length (board !! 0)
-- Pos(1,1) clockwise (0,1) (0,2) (1,2) (2,2) (2,1) (2,0) (1,0) (0,0)
--        positional (x,y-1) (x+1,y-1) (x+1,y) (x+1,y+1) (x,y+1) (x-1,y+1) (x-1,y) (x-1,y-1)
--              posAdj = [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]
-- 0 <= x <= #rows, 0 <= y <= #columns

-- (0,0) = board !! 0) !! 0
-- (1,2) = (board !! 1) !! 2
-- https://mail.haskell.org/pipermail/beginners/2014-January/013048.html
-- map over outer list, then process each inner element

map (f x) board
   f x = map (g y) x
     g y = if x == '*' then '*' 
           else if getVal (?,?) == 0 then ' '
                else show getVal (?,?)

g board row = [val | let str = board !! row, i <- [0..(length str)-1], let val = if str !! i == '*' then '*'
                                                              else if getVal (row,i) == 0 then ' '
                                                                    else head $ show $ getVal (row,i)]

f x = [line | j <- [0..(length board)-1], let line = g j]

(0,0) -> (#rows-1,#cols-1) (board !! row) !! col
posAdj = [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]
https://www.perplexity.ai/search/howto-generate-in-VnH15s0JTNO_S096JzIU.A?s=c

buildBoard board 
buildBoard [] _          = []
buildBoard _ (numRows-1) = []
buildBoard (x:xs) row = [j | i <- [0..(length x)-1], let j = if x !! i == '*' then '*'
                                               else if getVal (row,i) == 0 then ' '
                                                    else getVal (row,i)] : buildBoard xs (row+1)
  where numRows = length board
        numCols = length (board !! 0)

(board !! i) !! j == '*' = '*'
 
row : map (\x -> getVal (i,j)) (board !! i)
[j | i <- [0..2], let j = getVal (0,i)]

checkPos :: (Int,Int) -> [String] -> [(Int,Int)]
checkPos (row,col) board = filter (\(x,y) -> (x >= 0 && x < numRows) && (y >= 0 && y < numCols)) $ map (\x -> addTuple x (row,col)) posAdj
  where addTuple (a,b) (c,d) = (a+c,b+d)
        numRows = length board
        numCols = length (board !! 0)
        posAdj = [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

getVal :: (Int,Int) -> [String] -> Int
getVal (x,y) board = sum $ [1 | i <- posList, (board !! (fst i)) !! (snd i) == '*']
  where posList = checkPos (x,y) board