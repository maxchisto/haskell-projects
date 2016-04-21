data Log a = Log [String] a
    deriving Show

instance Functor Log where
    fmap f (Log xs a) = Log xs (f a)

instance Applicative Log where
    pure = returnLog
    (Log xs f) <*> (Log ys val) = Log (xs ++ ys) (f val)
    
instance Monad Log where
    return = returnLog
    (>>=) = bindLog

returnLog :: a -> Log a
returnLog = \x -> Log [] x
    
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f1 f2 = logC
    where (Log xs val1) = f1 x
          (Log ys val2) = f2 val1
          logC = Log (xs ++ ys) val2 

          
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs val) f = result
    where (Log ys val2) = f val
          result = Log (xs ++ ys) val2

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList init xs = foldl (>>=) (returnLog init) xs 

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"
        
-- Usage:
    -- GHCi> add1Log 3
    -- Log ["added one"] 4
    
    -- GHCi> mult2Log 3
    -- Log ["multiplied by 2"] 6
    
    -- GHCi> execLoggers 3 add1Log mult2Log
    -- Log ["added one","multiplied by 2"] 8
    
    -- GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
    -- Log ["added one","multiplied by 2","multiplied by 100"] 800