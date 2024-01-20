
module DiceTypes exposing (..)

type Dice = Face1 number | Face2 number | Face3 number | Face4 number | Face5 number | Face6 number
type alias DicePair = { firstDice: Dice, secondDice: Dice}

luckyRoll: DicePair -> String
luckyRoll (DicePair firstDice secondDice) = 
if (firstDice == Face6)  then
     if(secondDice == Face6) then
        



