module CryptoSquare (encode) where

import Data.Char (toLower,isAlpha,isAlphaNum)

data Square = Square Int Int
cols (Square r c) = c 

encode :: String -> String
encode [] = []
encode xs = createMessage (cols squareSize) fillSquare
  where strNorm = normalize xs
        squareSize = getSquareSize (length strNorm)
        fillSquare = createSquare (cols squareSize) strNorm

normalize :: String -> String
normalize xs = f . g $ xs
  where f = map toLower
        g = filter (isAlphaNum)

getSquareSize :: Int -> Square
getSquareSize lengthStr = getColumns rows lengthStr 
  where rows = floor (sqrt (fromIntegral lengthStr))

getColumns :: Int -> Int -> Square
getColumns rows lengthStr = if check == [] then getColumns (rows + 1) lengthStr
                            else Square rows (minimum check)
                            where check = filter (\c -> rows * c >= lengthStr) [rows..rows+1]

createSquare :: Int -> String -> [String]
createSquare _ [] = []
createSquare c xs = createLine : createSquare c (drop c xs)
  where createLine = if length xs >= c then newRow else (newRow ++ replicate (c - length xs) ' ')
        newRow = take c xs

createMessage :: Int -> [String] -> String
createMessage c xs = init $ concat [code | i <- [0..(c-1)], let code = (foldr (\x acc -> (take 1 (drop i x)) ++ acc) [] xs) ++ " "]
