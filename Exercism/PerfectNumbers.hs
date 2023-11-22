module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
    | sumOfFactors == n = Just Perfect
    | sumOfFactors > n = Just Abundant
    | sumOfFactors < n = Just Deficient
  where sumOfFactors = sum $ factors n
  

-- https://www.perplexity.ai/search/algorithm-to-find-5Nqs9i8iTiC0fn3fHDmAqg?s=c
factors :: Int -> [Int]
factors n = [x | x <- [1..n `div` 2], n `mod` x == 0]