data Coord a = Coord a a
    deriving (Show)

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) = Coord xCoord yCoord
    where   xCoord = ((fromIntegral x) + 0.5)*width
            yCoord = (fromIntegral y)*width + width/2

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = Coord x_id y_id
    where   x_id = (round $ x/width - 1/2)::Int
            y_id = (round $ y/width - 1/2)::Int
