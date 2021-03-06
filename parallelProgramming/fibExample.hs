import Control.Parallel
import Control.Monad
import Control.Monad.Par
-- import Control.Parallel.Strategies
import Text.Printf
 
-- takes a function which in turn takes two args, takes two args, and executes the function
parExec :: (a -> a -> a) -> a -> a -> a
parExec f a b = b `par` ( a `pseq` (f a b))
 
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
--fib n = parExec (+) l r
fib n = (+) l r
    where
        l = fib (n-1)
        r = fib (n-2)


-- implementation based on Par monad
parFib1 :: Int -> Int
parFib1 0 = 0
parFib1 1 = 1
parFib1 n = runPar $ do
    i <- new
    j <- new
    fork (put i (fib $ n-1))
    fork (put j (fib $ n-2))
    a <- get i
    b <- get j
    return (a+b)


-- Implementation based on parMap / Par monad 
parFib2 :: Int -> Int
parFib2 0 = 0
parFib2 1 = 1
parFib2 n = sum $ runPar $ parMap fib [n-2,n-1]

 
main = forM_ [2..45] $ \i ->
            printf "n=%d => %d\n" i (parFib1 i)