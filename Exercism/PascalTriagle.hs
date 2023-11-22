https://www.perplexity.ai/search/eb372a79-6628-449d-b3b6-bc7206ce5c7f?s=u
https://www.perplexity.ai/search/give-me-a-93QfysKQShuRnd4l6EKZWQ?s=c

-- Define a function to generate the nth row of Pascal's triangle
pascalRow :: Int -> [Int]
pascalRow 0 = [1]
pascalRow n = zipWith (+) (prev ++ [0]) ([0] ++ prev)
  where prev = pascalRow (n-1)

-- Define a function to generate the entire Pascal's triangle up to the nth row
pascalTriangle :: Int -> [[Int]]
pascalTriangle n = map pascalRow [0..n]


---
{-
rows :: Int -> [[Integer]]
rows x
  | x == 0 = []
  | x == 1 = [[1]]
  1 x == rows (x-1) : 
         map (\(r,c) -> rows r !! c) getVal (x-1)

getVal row = [(r, c) | r <- [row-1], c <- [col-1, col], c >= 0, c <= row]
row 1 (0, -1) + (0,0) (0,-1) + (0,0)
row 2 (1,-1) + (1,0) (1,0) + (1,1) (1,1) + (1,2)
  row - 1 
row col = [(r, c) | r <- [row-1], c <- [col-1, col], c >= 0, c <= row-1]
map (\col -> row col) [0..row-1]

tuples row = map (\col -> buildRow col) [0..row-1]
Prelude|   where buildRow col = [(r, c) | r <- [row-1], c <- [col-1, col], c >= 0, c < row-1]
Prelude|         -- row = 2
-}