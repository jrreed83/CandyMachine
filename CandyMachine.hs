module CandyMachine where

     data State s a = State {run::s -> (s,a)}
     
     data Machine = Machine {locked::Bool, candies::Int, coins::Int} deriving Show

     data Input = Coin | Turn deriving (Eq, Show)

     action :: Input -> State Machine (Int,Int)
     action x = State $ 
          \machine -> step x machine 
     
     step :: Input -> Machine -> (Machine, (Int,Int))
     step Coin (Machine locked candies coins) 
          | candies == 0                  = (Machine locked 0 coins, (0,coins))
          | locked == True && candies > 0 = (Machine False candies (coins+1), (candies,coins+1)) 
          | otherwise                     = (Machine locked candies coins, (candies, coins))
     step Turn (Machine locked candies coins)
          | locked == False && candies > 0 = (Machine True (candies-1) coins, (candies-1,coins))
          | candies == 0                   = (Machine locked 0 coins, (0,coins))
          | otherwise                      = (Machine locked candies coins, (candies, coins))

     andThen :: State Machine (Int,Int) -> State Machine (Int,Int) -> State Machine (Int,Int) 
     m1 `andThen` m2 = State $
          \s0 ->
               let (s1,t1) = (run m1) s0
                   (s2,t2) = (run m2) s1
               in  (s2,t2)     
   
     simulateMachine :: [Input] -> State Machine (Int,Int)
     simulateMachine (h:t) = action h `andThen` simulateMachine t
     simulateMachine []    = State $ \machine -> (machine, (candies machine, coins machine))      
