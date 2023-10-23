interpreter :: [String] -> [String]
interpreter [] = []
interpreter commands = readInput origin commands
    where origin = [0,0]

readInput :: [Int] -> [String] -> [String]
readInput xy [] = []  
readInput xy (x:xs) 
      | x == "printX" = ((map show xy) !! 0):readInput xy xs  -- write x value (xy !!0) to string 
      | x == "printY" = ((map show xy) !! 1):readInput xy xs  -- write y value (xy !!1) to string 
      | otherwise     = readInput newPos xs 
      where newPos = applyInput xy x
   
applyInput :: [Int] -> String -> [Int]
applyInput xy input = case input of
      "up"    -> zipWith (+) xy upMove
      "down"  -> zipWith (+) xy downMove
      "left"  -> zipWith (+) xy leftMove
      "right" -> zipWith (+) xy rightMove
    where upMove    = [0,1]
          downMove  = [0,(-1)]
          leftMove  = [(-1),0]
          rightMove = [1,0]