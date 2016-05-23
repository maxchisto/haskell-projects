data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

insert :: Char -> Tree Char -> Tree Char
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node depth Leaf y Leaf)
    = Node (depth+1) (insert x Leaf) y Leaf

insert x (Node depth Leaf y rightSTree@(Node depthR _ _ _))
    = Node (depthR+1) (insert x Leaf) y rightSTree

insert x (Node depth leftSTree@(Node depthL _ _ _) y Leaf)
    = Node (depthL+1) leftSTree y (insert x Leaf)

insert x (Node depth leftSTree@(Node depthL _ _ _) y rightSTree@(Node depthR _ _ _))
    | depthL < depthR     = Node (depthR+1) (insert x leftSTree) y rightSTree
    | otherwise    = Node (depthL+1) leftSTree y (insert x rightSTree)

build :: String -> Tree Char
build = foldr insert Leaf


-- Task: use foldr to xor all element of the list
xorGate :: Bool -> Bool -> Bool
xorGate x y =  not (((not (not (x && y)) && x)) && (not (not (x && y)) && y))

xor :: [Bool] -> Bool
xor xs = foldr xorGate False xs 

