data Log a = Log [String] a
    deriving Show

instance Functor Log where
    fmap f (Log xs a) = Log xs (f a)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f1 f2 = logC
    where (Log xs val1) = f1 x
          (Log ys val2) = f2 val1
          logC = Log (xs ++ ys) val2 
          
          
-- Usage:
    -- GHCi> let add1Log = toLogger (+1) "added one"
    -- GHCi> add1Log 3
    -- Log ["added one"] 4

    -- GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
    -- GHCi> mult2Log 3
    -- Log ["multiplied by 2"] 6
    
    -- GHCi> execLoggers 3 add1Log mult2Log
    -- Log ["added one","multiplied by 2"] 8