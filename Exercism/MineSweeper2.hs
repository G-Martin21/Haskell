module Minesweeper (annotate) where

import Data.Char (isSpace)

annotate :: [String] -> [String]
annotate board
  | all (all isSpace) board = board
  | all (all (\x -> x == '*')) board = board
  | otherwise = buildBoard board 
 
-- buildBoard board = [line | j <- [0..(length board)-1], let line = g j]
--  where g row = [column | let str = board !! row, i <- [0..(length str)-1], let column = g' i str row]
--        g' i str row = if (str !! i) == '*' then '*'
--                       else if getVal (row,i) board == 0 then ' '
--                            else head $ show $ getVal (row,i) board

buildBoard :: [String] -> [String]
buildBoard board = map buildRow (zip [0..] board)
  where buildRow (row, str) = map (buildCell row) (zip [0..] str)
        buildCell row (col, '*') = '*'
        buildCell row (col, _) | getVal (row, col) board == 0 = ' '
                               | otherwise = head $ show $ getVal (row, col) board

-- getVal :: (Int,Int) -> [String] -> Int
-- getVal (x,y) board = sum $ [1 | i <- posList, (board !! (fst i)) !! (snd i) == '*']
--  where posList = checkPos (x,y) board

getVal :: (Int, Int) -> [String] -> Int
getVal (row, col) board = sum [1 | pos <- posList, '*' `elem` [board !! r !! c | (r, c) <- [pos]]]
  where posList = checkPos (row, col) board

-- checkPos :: (Int,Int) -> [String] -> [(Int,Int)]
-- checkPos (row,col) board = filter (\(x,y) -> (x >= 0 && x < numRows) && (y >= 0 && y < numCols)) $ map (\x -> addTuple x (row,col)) posAdj
--   where addTuple (a,b) (c,d) = (a+c,b+d)
--         numRows = length board
--         numCols = length (board !! 0)
--         posAdj = [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

checkPos :: (Int, Int) -> [String] -> [(Int, Int)]
checkPos (row, col) board = [(r, c) | r <- [row-1, row, row+1], c <- [col-1, col, col+1], r >= 0, r < numRows, c >= 0, c < numCols, (r, c) /= (row, col)]
   where numRows = length board
         numCols = length (head board)

---

Refactoring

https://www.perplexity.ai/search/can-this-haskell-OQF0AsF2SV6RK0ZR_voz2Q?s=c

buildBoard :: [[Char]] -> [[Char]]
buildBoard board = map buildRow (zip [0..] board)
  where buildRow (row, str) = map (buildCell row) (zip [0..] str)
        buildCell row (col, '*') = '*'
        buildCell row (col, _) | getVal (row, col) board == 0 = ' '
                               | otherwise = head $ show $ getVal (row, col) board

https://www.perplexity.ai/search/can-this-haskell-OQF0AsF2SV6RK0ZR_voz2Q?s=c

getVal :: (Int, Int) -> [String] -> Int
getVal (row, col) board = sum [1 | pos <- posList, '*' `elem` [board !! r !! c | (r, c) <- [pos]]]
  where posList = checkPos (row, col) board


checkPos :: (Int, Int) -> [String] -> [(Int, Int)]
checkPos (row, col) board = [(r, c) | r <- [row-1, row, row+1], c <- [col-1, col, col+1], r >= 0, r < numRows, c >= 0, c < numCols, (r, c) /= (row, col)]
  where numRows = length board
        numCols = length (head board)
