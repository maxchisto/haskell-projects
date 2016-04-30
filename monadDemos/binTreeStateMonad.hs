-- Given an arbitrary tree, transform it to a tree of integers 
-- in which the original elements are replaced by natural numbers, 
-- starting from 0

-- Usage: `numberTree (Branch (Leaf ()) () (Branch (Leaf ()) () (Leaf ()) ))`
-- result: Branch (Leaf 0) 1 (Branch (Leaf 2) 3 (Leaf 4))

import Control.Monad.State

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
    deriving (Show)


numberTree :: Tree () -> Tree Int
numberTree tree = evalState (indexTree tree) 0

indexTree :: Tree a -> State Int (Tree Int)
indexTree (Leaf x)
    = do 
        count <- get
        put (count+1)
        return (Leaf count) 
        
indexTree (Branch tree1 x tree2)
    = do 
        newtTree1 <- indexTree tree1
        count <- get
        put (count+1)
        newTree2 <- indexTree tree2
        return (Branch newtTree1 count newTree2)


          

                    