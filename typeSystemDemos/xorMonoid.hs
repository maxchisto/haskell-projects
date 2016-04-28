newtype Xor = Xor Bool 
    deriving Show

xor :: Xor -> Xor -> Xor
xor (Xor a) (Xor b) = Xor ( (a || b) && (not a || not b) ) 

instance Monoid Xor where
    mempty = Xor False
    mappend = xor
    
-- Now try mconcat
-- Usage: mconcat [Xor True, Xor True, Xor False, Xor True]