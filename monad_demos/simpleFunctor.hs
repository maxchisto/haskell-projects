data Point3D a = Point3D a a a 
    deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)
    
shift :: Int -> Point3D Int -> Point3D Int
shift x p = fmap (+x) p

scale :: Int -> Point3D Int -> Point3D Int
scale x p = fmap (*x) p

-- Usage: scale 3 (Point3D 1 1 1)