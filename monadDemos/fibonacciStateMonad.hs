-- Fibonacci calculator based on the State monad

import Control.Monad.State

-- Tuple represents two last number in the series.
-- When executed, both numbers get bumped up to the next one it the series.
-- For example (3,5) becomes (5,8)
fibStep :: State (Integer,Integer) ()
fibStep = do (a,b) <- get
             put (b, a+b)
             return ()

-- Helper function which executes a monad n times             
execStateN :: Integer -> State s a -> s -> s
execStateN n stateMonad init = execState (replicateM (fromIntegral n) stateMonad) init

fib :: Integer -> Integer
fib x = fst $ execStateN x fibStep (0,1)