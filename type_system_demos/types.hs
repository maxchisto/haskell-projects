data Result' = Fail Int | Success 

instance Show Result' where
    show (Success) = show "Success"
    show (Fail x) = show "Fail: " ++ show x

doSomeWork' :: Maybe Int -> Result'
doSomeWork' (Just x) = Success
doSomeWork' _ = Fail 1


data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

instance Show Bit where
    show Zero = show 0
    show One = show 1
    
instance Show Sign where
    show Minus = show "-"
    show _ = show ""
    
instance Show Z where
    show (Z sign bits) = show sign ++ " " ++ show bits
    
addBits :: Bit -> Bit -> (Bit, Bit)        
addBits Zero Zero = (Zero, Zero)
addBits One Zero = (One, Zero)
addBits Zero One = (One, Zero)
addBits One One = (Zero, One)

addAllBits :: [Bit] -> [Bit] -> [Bit]
addAllBits xs [] = xs
addAllBits [] ys = ys
addAllBits (x:xs) (y:ys) = (addBits x y) : (addAllBits xs ys)

    
add :: Z -> Z -> Z
add (Z sign1 xs) (Z sign2 ys) = Z finalSign bits 
    where result = 
    
add (Z sign1 (x:xs)) (Z sign2 ([])) = Z sign1 (x:xs)
add (Z sign1 ([])) (Z sign2 (y:ys)) = Z sign2 (y:ys)

mul :: Z -> Z -> Z
mul = undefined