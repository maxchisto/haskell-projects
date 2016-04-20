data Nat = Zero | Suc Nat
    deriving (Show)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Int -> Nat
toNat 0 = Zero
toNat x = Suc (toNat (x-1))

add :: Nat -> Nat -> Nat
add x y = toNat $ (fromNat x) + (fromNat y)

mul :: Nat -> Nat -> Nat
mul x y = toNat $ (fromNat x) * (fromNat y)

fac :: Nat -> Nat
fac x = toNat result
    where facNum = fromNat x
          result = facNum*(facNum+1) `quot` 2