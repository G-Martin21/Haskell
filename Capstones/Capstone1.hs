-- Modelling objects with Functional Programming

-- constructor using closures
cup oz = \message -> message oz

-- instance
coffeeCup = cup 12

-- accesor: send message -> get the state
getOz aCup = aCup (\oz -> oz)
-- accesor: change state -> object newState
drink aCup ozDrank = if ozDiff >= 0
                     then cup (oz - ozDrank)
                     else cup 0
 where oz = getOz aCup
       ozDiff = oz - ozDrank

-- helper funtion is empty?
isEmpty aCup = (getOz aCup) == 0
-- helper function take n drinks
afterManySips = foldl drink coffeeCup [1,1,1,1,1]
--
takeXSips n = foldl drink coffeeCup sips where sips = take n (repeat 1)