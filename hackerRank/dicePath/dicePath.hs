-- https://www.hackerrank.com/challenges/dice-path

-- Problem Statement
-- You are given an MxN grid and a 6 sided dice starting at the point (1, 1). 
-- You can only move dice toward right or down by rotating it in the respective direction. 
-- The value of the dice is the number of pips on the top face of it.

-- Note
-- The sum of pips at each pair of opposing sides is always 7.

-- Input
-- The first line contains an integer, T, which denotes the number of test cases. T lines follow.
-- Each of these lines contains two space separated integers, M N, which represent the final point in the grid.

-- Output
-- For each test case, print the sum of maximal path to (M, N). 

{-# LANGUAGE FlexibleInstances #-}

import Data.Map.Strict as Map
import Data.Maybe


type DiceState = (Int, Int, Int)
type DicePath = (DiceState, Sum)

type Coords = (Int, Int)
type Solution = Map Coords (DiceState, Sum) 

type Problem = Coords
type Sum = Int

-- Dynamic programming problem table
problemTable m n = do
    x <- [1..m]
    y <- [1..n]
    return (x,y)

initialDiceState = (1,2,4)

diceValue :: DiceState -> Int
diceValue (top, front, right) = top

-- Simulates rolling the dice one step down the board
rollDown :: DicePath -> DicePath
rollDown ((-1,-1,-1),_) = ((-1,-1,-1),0)
rollDown ((top, front, right),x) = ((7 - front, top, right),7-front+x)

-- Simulates rolling the dice one step to the right
rollRight :: DicePath -> DicePath
rollRight ((-1,-1,-1),_) = ((-1,-1,-1),0)
rollRight ((top, front, right),x) = ((7 - right, front, top),7-right+x)


rollRightRight  = (rollRight . rollRight)
rollDownDown    = (rollDown . rollDown)
rollDownRight   = (rollRight . rollDown)
rollRightDown   = (rollDown . rollRight)


-- instance Ord DicePath where
--     (_,x) > (_,y) = x > y
--     (_,x) <= (_,y) = x <= y
    


-- The basis for dynamic programming algorithm.
-- Calculates the solution for the current step based on solutions for the previous steps
diceStep :: Solution -> Problem -> Solution
diceStep subSolution (1,1) = insert (1,1) (initialDiceState, 1) subSolution
diceStep subSolution (2,1) = insert (2,1) ((5,1,4),6) subSolution
diceStep subSolution (1,2) = insert (1,2) ((3,2,1),4) subSolution
diceStep subSolution (x,y) = insert (x,y) (newState, newSum) subSolution
    where  maybeLeft = (Map.lookup (x-2,y) subSolution)
           maybeTop  = (Map.lookup (x,y-2) subSolution)
           maybeDiagonal = (Map.lookup (x-1,y-1) subSolution)
          
           leftState = fromMaybe ((-1,-1,-1),0) maybeLeft
           topState = fromMaybe ((-1,-1,-1),0) maybeTop
           diagonalState = fromMaybe ((-1,-1,-1),0) maybeDiagonal
           
           stateA = rollRightRight leftState
           stateB = rollDownDown topState
           stateC = rollDownRight diagonalState
           stateD = rollRightDown diagonalState
           
           (newState, newSum) = maximum [stateA, stateB, stateC, stateD]
            


-- Here we apply our step function to every step                   
calculateSum :: Int -> Int -> Int
calculateSum m n = maxSum 
    where solutionMap = Prelude.foldl diceStep empty (problemTable m n)
          maybeResult = Map.lookup (m,n) solutionMap
          (finalState, maxSum) = fromMaybe ((0,0,0),0) maybeResult




----------------------
-- HACKER RANK CODE --
----------------------

readInts :: IO [Int]
readInts = fmap (Prelude.map read.words) getLine

runTestCase 0 = return ()
runTestCase n = 
    do
        ints <- readInts
        
        putStrLn $ show (calculateSum (head ints) (last ints) )
        runTestCase (n-1)


-- | The main entry point.
main :: IO ()
main = do
    n <- getLine
    runTestCase $ read n                                            