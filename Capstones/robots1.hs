-- threeRoundFight :: Robot -> Robot -> Robot
threeRoundFight robotA robotB =
  let
    -- First round
    result1 = fight robotA robotB
    -- Second round
    result2 = fight robotB result1
    -- Third round
    result3 = fight result1 result2
  in
    -- Return the winner after three rounds
    if getHP result3 > 0 then result3 else result2


---

oneRound = fight killerRobot gentleGiant
twoRounds = (\b -> fight gentleGiant b) oneRound
threeRounds = (\b -> fight killerRobot b) twoRounds
printRobot threeRounds


oneRoundsFight = (\killerRobot ->
                     (\robotB -> fight robotB))