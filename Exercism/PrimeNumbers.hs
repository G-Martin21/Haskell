module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just (last $ primes n)  -- alternative !! (n-1)


-- https://www.perplexity.ai/search/in-this-haskell-BxjwBbUqQgeOBorNjk2nPg?s=c
-- https://www.perplexity.ai/search/in-haskell-how-rGM8RSWxQT.OXzZJAeSokQ?s=c

primes :: Int -> [Integer]
primes n = take n [x | x <- 2:[3,5..], isPrime x]

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) (takeWhile (\x -> x*x <= n) [2..])


-- Solution 1, not valid for large numbers
-- last $ take n [i | i <- [2..], testPrime i 2])
-- testPrime :: Integer -> Integer -> Bool
-- testPrime n j 
--  | j < (div n 2) + 1 = if mod n j == 0 then False else testPrime n (j+1)
--  | otherwise = True

-- Source https://atechdaily.com/posts/algorithm-for-Prime-Number?q=Education+Loan#gsc.tab=0&gsc.q=Education%20Loan&gsc.page=1
