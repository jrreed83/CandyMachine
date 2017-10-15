module CandyMachine where

     data State s a = State {run::s -> (s,a)}
     
     data Machine = Machine {locked::Bool, candies::Int, coins::Int} deriving Show

     data Input = Coin | Turn deriving (Eq, Show)

     action :: Input -> State Machine (Int,Int)
     action x = State { run = \machine -> update x machine }
     
     update :: Input -> Machine -> (Machine, (Int,Int))
     update Coin (Machine locked candies coins) 
          | candies == 0                  = (Machine locked 0 coins, (0,coins))
          | locked == True && candies > 0 = (Machine False candies (coins+1), (candies,coins+1)) 
          | otherwise                     = (Machine locked candies coins, (candies, coins))
     update Turn (Machine locked candies coins)
          | locked == False = (Machine True 0 coins, (0,coins))
          | candies == 0    = (Machine locked 0 coins, (0,coins))
          | otherwise       = (Machine locked candies coins, (candies, coins))

     simulateMachine :: [Input] -> State Machine (Int,Int)
     simulateMachine (h:t) = action h
