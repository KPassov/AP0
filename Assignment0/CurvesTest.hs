import Test.QuickCheck
import System.Random
{- import Test.QuickCheck.Gen -}
import Curves


dePoint :: Point -> (Double,Double)
dePoint (Point (a,b)) = (a,b)

checkPoint :: (Double, Double) -> Bool
checkPoint (a,b) = dePoint(point(a,b)) == (a,b)

deCurve :: Curve -> (Point, [Point])
deCurve (Curve p ps) = (p, ps)

checkCurve :: Bool
checkCurve = do curve@(p, ps) <- createCurve
                return $ (deCurve(curve) == (p, ps))

createCurve :: IO Curve
createCurve = do p <- createPoint 
                 ps <- points 
                 return $ curve p ps 
        where points = do rand <- randomRIO(1,10::Int) 
                          sequence $ take rand (repeat createPoint)

createPoint :: IO Point
createPoint = do [_, x, y] <- a
                 return $ point (x,y) 
        where a = fmap (take 3) (sample' arbitrary :: IO [Double])
    --For some reason the first element is always 0.0 so i use the secound and third 

{- checkRotate :: Curve -> Double ->   -}

