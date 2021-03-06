module TestCandyMachine where
     import Test.HUnit
     import CandyMachine 

     -- Test in example
     inputs = [Coin,Turn,Coin,Turn,Coin,Turn,Coin,Turn]
     fn     = CandyMachine.run $ simulateMachine inputs
     result = snd $ fn (Machine True 5 10)
     test1  = TestCase (assertEqual "test" result (1,14))
    
     tests = TestList [TestLabel "test1" test1]

     runTests = runTestTT tests
