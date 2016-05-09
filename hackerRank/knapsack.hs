-- Functional implementation of a dynamic programming algorithm for the classic "knapsack" problem

import Data.Map.Strict as Map
import Data.Maybe

type Weight = Int
type Value = Int
type Id = Int
type Item = (Value, Weight)
type Problem = ((Id,Item),Weight)
type Solution = Map (Id,Weight) Value

items = [(4, 3),
         (3, 2),
         (2, 4),
         (3, 4)] :: [Item]
 
totalWeight = 6 :: Weight

start = 1 :: Id

indexedItems = (zip [start..] items)
         
-- Table of sub-problems that we need to solve in order to arrive at the filan answer
problemTable = do
     (id,(vi,wi)) <- indexedItems
     w <- [1..totalWeight]
     return ((id,(wi,vi)),w)

-- A stepping function.
-- On every step:
--     1) it takes a table of already solved sub-problems
--     2) solves the current sub-problem
--     3) return updated table of solved sub-problems        
knapStep :: Solution -> Problem -> Solution
knapStep subSolution ((id,(vi,wi)),weight)  = insert (id, weight) maxVal subSolution
    where value | wi > weight  = 0
                | otherwise  = vi
          
          resid | wi > weight  = 0
                | otherwise  = weight - wi

          previousValue = fromMaybe 0 (Map.lookup (id-1,weight) subSolution)
                                                        
          residualValue = fromMaybe 0 (Map.lookup (id-1,weight-resid) subSolution)
                                              
          maxVal = max previousValue (value + residualValue)


-- The solution is nothing more than applying our stepping function to every sub-problem  
solution = Prelude.foldl knapStep empty problemTable

    



    
    