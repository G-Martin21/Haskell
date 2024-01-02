import Data.List (isInfixOf, sort,nub)

-- Sorry for the name of the function.
inArray :: [String] -> [String] -> [String]
inArray [] _ = []
inArray _ [] = []
-- Refactor solution
-- https://www.perplexity.ai/search/refactor-this-haskell-OGM6Zym_Rj21Jz0fXK70XA?s=c
inArray a1 a2 = sort $ nub [x | x <- a1, any (x `isInfixOf`) a2]
-- equivalent to filter (\x -> any (x `isInfixOf`) a2) ["arp", "trong"]
-- [f x | x <- lis, p x]
-- is equivalent to
-- map f (filter p lis)

{- solution 1
inArray a1 a2 = removeDuplicates $ sort $ checkStr a1 a2 []

checkStr :: [String] -> [String] -> [String] -> [String]
checkStr [] _ acc = acc
checkStr (x:xs) a2 acc = if any (\str -> any (str `isInfixOf`) a2) [x] then checkStr xs a2 (x : acc)
                      else checkStr xs a2 acc
                      
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/=x) xs)
-}