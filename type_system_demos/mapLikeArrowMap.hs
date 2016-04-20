import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)
    
newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }
     

instance MapLike ArrowMap where
    empty = ArrowMap (\x->Nothing)
    
    lookup k (ArrowMap f) = f k
        
    insert k v (ArrowMap f) = ArrowMap (\x -> if k==x then Just v else f x)
    
    delete k (ArrowMap f) = ArrowMap (\x -> if k==x then Nothing else f x)


-- Demo: lookup 2 $ insert 2 7 $ delete 2 $ insert 3 4 $ insert 2 3 $ insert 1 2 (ArrowMap (\x->Nothing))