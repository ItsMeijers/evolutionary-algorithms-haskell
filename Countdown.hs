module Countdown where

  data Operation = Test

  newtype Numbers = Numbers { ns :: [Number]}

  data Number = HighNumber
              | LowNumber

  data HighNumber = TwentyFive
                  | Fifty
                  | SeventyFive
                  | Hundred

  data LowNumber = One
                 | Two
                 | Three
                 | Four
                 | Five
                 | Six
                 | Seven
                 | Eight
                 | Nine
                 | Ten

  highNumbers :: [Int]
  highNumbers = [25, 50, 75, 100]

  lowNumbers :: [Int]
  lowNumbers = [1..10]
