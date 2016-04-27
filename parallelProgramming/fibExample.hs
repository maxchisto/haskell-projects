import Control.Parallel
import Control.Monad
import Text.Printf
 
-- takes a function which in turn takes two args, takes two args, and executes the function
parExec :: (a -> a -> a) -> a -> a -> a
parExec f a b = b `par` ( a `pseq` (f a b))
 
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = parExec (+) l r
    where
        l = fib (n-1)
        r = fib (n-2)
 
main = forM_ [2..45] $ \i ->
            printf "n=%d => %d\n" i (fib i)