

import Control.Monad.State

fibStep :: State (Integer,Integer) ()
fibStep = do (a,b) <- get
             put (b, a+b)
             return ()
             
execStateN :: Integer -> State s a -> s -> s
execStateN n stateMonad init = execState (replicateM (fromIntegral n) stateMonad) init

fib :: Integer -> Integer
fib x = fst $ execStateN x fibStep (0,1)