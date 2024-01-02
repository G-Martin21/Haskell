-- refactor 
-- https://www.perplexity.ai/search/refactor-this-haskell-UX.caQsASM.A8X6ZTMwZbw?s=c

perimeter :: Integer -> Integer
perimeter n = 4 * fib (n + 1)
  where fib 0 = 0
        fib 1 = 1
        fib m = fib (m - 1) + fib (m - 2)

-- the solution is wrong! change to 4 * (sum $ map fib [1..n+1]) but as state in the MOOC this solution
-- is inefficient

{-
2.1
This type of recursion where a function just directly calls itself with different arguments is called tail recursion.
 As you’ve seen above, tail recursion corresponds to loops. This is why tail recursion is often fast: the
  compiler can generate a loop in machine code when it sees tail recursion.

  Python:

def fibonacci(n):
    a = 0
    b = 1
    while n>1:
        c = a+b
        a = b
        b = c
        n = n-1
    return b
-}
-- solution from Haskell MOOC
perimeter :: Integer -> Integer
perimeter n = 4 * (sum $ map (perimeter' 0 1) [1..n+1])

perimeter' :: Integer -> Integer -> Integer -> Integer
perimeter' a b 1 = b
perimeter' a b n = perimeter' b (a+b) (n-1)