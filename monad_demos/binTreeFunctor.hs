data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) 
    deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f <$> x)
    fmap g (Branch l c r) = Branch (g <$> l) (g <$> c) (g <$> r)