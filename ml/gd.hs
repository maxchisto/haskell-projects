{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V


type DataPoint = (Float,Float)
type DataSet = [DataPoint]
type MCoeff = Float
type BCoeff = Float
type LearningRate = Float


costFunction :: (MCoeff, BCoeff) -> DataPoint -> Float
costFunction (m, b) (x ,y) = (y - (m * x + b)) ** 2

totalCost :: (MCoeff, BCoeff) -> DataSet -> Float
totalCost (m, b) xys = total
    where costs = map (costFunction (m, b)) xys
          total = (foldr (+) 0 costs) / fromIntegral (length xys) 


bGradient :: (MCoeff, BCoeff) -> DataPoint -> Float
bGradient (m, b) (x ,y)  = -2 * (y - ((m * x) + b))

mGradient :: (MCoeff, BCoeff) -> DataPoint -> Float
mGradient (m, b) (x ,y)  = -2 * x * (y - ((m * x) + b))

bGradientTotal :: (MCoeff, BCoeff) -> DataSet -> Float
bGradientTotal (m, b) xys = total
    where costs = map (bGradient (m, b)) xys
          total = (foldr (+) 0 costs) / fromIntegral (length xys)

mGradientTotal :: (MCoeff, BCoeff) -> DataSet -> Float
mGradientTotal (m, b) xys = total
    where costs = map (mGradient (m, b)) xys
          total = (foldr (+) 0 costs) / fromIntegral (length xys)


gradientStep :: DataSet -> LearningRate -> (MCoeff, BCoeff) -> (MCoeff, BCoeff)
gradientStep xys alpha (m, b) = (newMCoeff, newBCoeff)
    where bCost = bGradientTotal (m,b) xys
          mCost = mGradientTotal (m,b) xys
          newBCoeff = b - (alpha * bCost)
          newMCoeff = m - (alpha * mCost)


results :: DataSet -> (MCoeff, BCoeff) -> IO (MCoeff, BCoeff)
results dataSet (m, b) = do
    let currentTotalCost = totalCost (m, b) dataSet
    putStrLn $ show (m, b) ++ "; error=" ++ show currentTotalCost
    let (newM, newB) = gradientStep dataSet 0.0001 (m, b)
    let newTotalCost = totalCost (newM, newB) dataSet
    if abs(currentTotalCost - newTotalCost) < 0.0001
        then return (newM, newB)
        else results dataSet (newM, newB)


main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> do 
            _ <- results ((V.toList v) :: DataSet) (0,0)
            return ()
    
    return ()