data Point3D a = Point3D a a a 
    deriving Show

data GeomPrimitive a = Point (Point3D a) | Line (Point3D a) (Point3D a)
    deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

instance Functor GeomPrimitive where
    fmap f (Point p3d) = Point (fmap f p3d) 
    fmap f (Line p1 p2) = Line (fmap f p1) (fmap f p2)

    
shift :: Int -> Point3D Int -> Point3D Int
shift x p = fmap (+x) p

scale :: Int -> Point3D Int -> Point3D Int
scale x p = fmap (*x) p

-- Usage: scale 3 (Point3D 1 1 1)