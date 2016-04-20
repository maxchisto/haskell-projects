import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup key (ListMap []) = Nothing
    lookup key (ListMap ((k,v):pairs)) | k == key = Just v
                                       | otherwise = lookup key (ListMap pairs) 
     
    insert key value (ListMap pairs) = ListMap ((key,value):(filter ((/=key).fst) pairs ))
    
    delete key (ListMap []) = ListMap []
    delete key (ListMap pairs) = ListMap (filter ((/=key).fst) pairs)

-- Demo: lookup 2 $ insert 2 7 $ delete 2 $ insert 3 4 $ insert 2 3 $ insert 1 2 (ListMap [])