module CandyMachine where

     data State s a = State (s -> (s,a))
     
     data Machine = Machine {locked::Bool, candies::Int, coins::Int} deriving Show

     data Input = Coin | Turn deriving (Eq, Show)

     action :: Input -> State Machine (Int,Int)
     action Coin = State $
          \machine -> switch Coin machine
               
     
     switch :: Input -> Machine -> (Machine,(Int,Int))
     switch Coin (Machine locked candies coins) 
          | candies == 0                  = (Machine locked 0 coins, (0,coins))
          | locked == True && candies > 0 = (Machine False candies (coins+1), (candies,coins+1)) 
          | otherwise                     = (Machine locked candies coins, (candies, coins))
