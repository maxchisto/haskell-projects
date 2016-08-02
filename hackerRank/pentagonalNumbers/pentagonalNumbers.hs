type Size = Int
type Count = Int
type Pentagon = (Size, Count)


-- https://www.hackerrank.com/challenges/pentagonal-numbers

calculate :: Int -> Count
calculate x = ((x*(x+1) `quot` 2)*3 - 2*x) :: Count

----------------------
-- HACKER RANK CODE --
----------------------

readInts :: IO [Int]
readInts = fmap (Prelude.map read.words) getLine

runTestCase 0 = return ()
runTestCase n = 
    do
        ints <- readInts
        
        putStrLn $ show (calculate (head ints))
        runTestCase (n-1)


-- | The main entry point.
main :: IO ()
main = do
    n <- getLine
    runTestCase $ read n  