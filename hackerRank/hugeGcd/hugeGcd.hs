import qualified Data.List as DL

factor :: Integer -> [Integer]
factor 0 = []
factor 1 = []
factor x = factorLoop x 2

factorLoop :: Integer -> Integer -> [Integer]
factorLoop x i
    | i >= x          = [i]
    | x `mod` i == 0  = i : factorLoop (div x i) 2
    | otherwise       = factorLoop x (i+1)
    
factorList :: [Integer] -> [Integer]
factorList [] = []
factorList (x:xs) = factor x ++ factorList xs

intersectWithAcc :: [Integer] -> [Integer] -> [Integer] -> [Integer]
intersectWithAcc [] _ acc = acc
intersectWithAcc _ [] acc = acc
intersectWithAcc (x:xs) (y:ys) acc
    | x < y  =  intersectWithAcc xs (y:ys) acc
    | x == y =  intersectWithAcc xs ys ([x] ++ acc)
    | x > y  =  intersectWithAcc (x:xs) ys acc


intersect xs ys = intersectWithAcc xs ys [1]

processLists :: [Integer] -> [Integer] -> Integer
processLists xs ys = result
    where newXs = DL.sort (factorList xs)
          newYs = DL.sort (factorList ys)
          overlap = intersect newXs newYs
          result = (foldr (*) 1 overlap) `mod` 1000000007

readInts :: IO [Integer]
readInts = fmap (Prelude.map read.words) getLine



main :: IO ()
main = do
    _ <- readInts
    xs <- readInts
    _ <- readInts
    ys <- readInts
    putStrLn $ show $ (processLists xs ys) 