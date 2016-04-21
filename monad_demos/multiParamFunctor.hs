data Entry k1 k2 v = Entry (k1, k2) v
    deriving Show
    
data Map k1 k2 v = Map [Entry k1 k2 v]
    deriving Show

instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)
    
instance Functor (Map k1 k2) where
    fmap f (Map xs) = Map (map (fmap f) xs)
    

-- Usage: fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]