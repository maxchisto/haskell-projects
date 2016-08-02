data Tree a = Empty | Node Int (Tree a) (Tree a)
    deriving (Show)


insert :: Tree Int -> Int -> Tree Int
insert (Empty) x = Node x Empty Empty
insert (Node val left right) x
    | x <= val  =  Node val (insert left x) right
    | otherwise =  Node val left (insert right x)


merge :: Tree Int -> Tree Int -> Tree Int
merge tree Empty = tree
merge Empty tree = tree
merge (Node val1 left1 right1) tree2 
    = insert (merge left1 (merge tree2 right1)) val1 

generateTree :: Tree Int
generateTree = foldl insert Empty [1..10]


printTree :: Tree Int -> IO ()
printTree (Empty) = putStr "_"
printTree (Node val left right)
    = do putStr "{"
         printTree left
         putStr $ show val
         printTree right
         putStr "}"


rotateLeft :: Tree Int -> Tree Int
rotateLeft (Empty) = Empty
rotateLeft (Node val Empty right) = insert right val
rotateLeft (Node val left right) = insert (merge right left) val