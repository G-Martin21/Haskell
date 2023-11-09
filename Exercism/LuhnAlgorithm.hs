module Luhn (isValid) where

import Data.Char (isSpace,isDigit,digitToInt)

isValid :: String -> Bool
isValid n
  | length cleanStr <= 1     = False
  | not (all isDigit cleanStr) = False
  | otherwise         = validateStr cleanStr
  where cleanStr = foldr (\x acc -> if not (isSpace x) then [x] ++ acc else acc) [] n

validateStr :: String -> Bool
validateStr str = if mod sumDigits 10 == 0 then True else False
  where evenPos = [digitToInt (reverse str !! i) | i <- [1,3..(length str-1)]]
        oddPos  = [digitToInt (reverse str !! i) | i <- [0,2..(length str-1)]]
        sumDigits = sum (map (\x -> if x*2 > 9 then x*2-9 else x*2) evenPos) + sum oddPos
