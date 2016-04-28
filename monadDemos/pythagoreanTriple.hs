-- Takes an Int and returns a set of triplets representing triange sides' lengths
-- s.t. a^2+b^2=c^2, a>0, b>0, c>0, c≤x, a<b 

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple n = do c <- [1..n]
                         b <- [1..c-1]
                         a <- [1..b-1]
                         True <- return (a^2 + b^2 == c^2)
                         return (a,b,c) 
