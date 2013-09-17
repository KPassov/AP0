module Curves where 
-- OLEKS -1: Consider limiting the module to not export garbage, i.e. your
-- module should _only_ export those functions that we ask for. See
-- http://www.haskell.org/tutorial/modules.html

import Text.Printf

data Point = Point (Double, Double)
    deriving (Show,Read,Ord)
    
instance Eq Point where
        Point(x1,y1) == Point(x2,y2) = bound x1 x2 == 0 && bound y1 y2 == 0
        Point(x1,y1) /= Point(x2,y2) = bound x1 x2 /= 0 || bound y1 y2 /= 0
        
bound :: Double -> Double -> Int
bound x y = truncate(x*100) - truncate(y*100)

    --Point(x1,y1) == Point(x2,y2) = (printf "%.2f %.2f" x1 y1 ::String)  == 
    --                                printf "%.2f %.2f" x2 y2
    --Point(x1,y1) /= Point(x2,y2) = (printf "%.2f %.2f" x1 y1 ::String)  /= 
    --                                printf "%.2f %.2f" x2 y2
-- OLEKS -3: From your BSc you should remember that the above is not a
-- satisfactory way of comparing floating point with an error margin. Consider
-- reading
-- http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
-- 
-- The problem in particular is that by multiplying a float by a number
-- DOES NOT give you a more precise float -- all you're doing is moving
-- the decimal point. Ideally you should not use == on floats. Ever.

data Curve = Curve Point [Point]

cmap :: (Point -> Point) -> Curve -> Curve
cmap f (Curve s ps) = Curve (f s) (map f ps)

data Axis = Vertical | Horizontal

point:: (Double, Double) -> Point
point (x,y) = Point (x,y)

curve:: Point -> [Point] -> Curve
curve p c = Curve p c

connect :: Curve -> Curve -> Curve
connect (Curve p1 c1) (Curve p2 c2) = Curve p1 (c1 ++ (p2:c2))

rotate :: Curve -> Double -> Curve
rotate c degree = cmap rotate' c 
            where radiant = degree * (pi/180)
                  (sinr,cosr) = (sin radiant,cos radiant)
                  rotate' = (\(Point(x,y)) -> Point (x*cosr + y*sinr,
                                                     y*cosr - x*sinr))


translate :: Curve -> Point -> Curve
translate c@(Curve (Point(cx,cy)) _) (Point (px,py)) = cmap move c
            where move    = (\(Point (x1,x2)) -> Point (x1-dx,x2-dy))
                  (dx,dy) = (cx-px,cy-py)


reflect :: Curve -> Axis -> Double -> Curve
reflect c Vertical   d = cmap (\(Point(x,y)) -> Point(x - (x-d)*2,y)) c
reflect c Horizontal d = cmap (\(Point(x,y)) -> Point(x,y - (y-d)*2 )) c


bbox :: Curve -> (Point, Point)
bbox (Curve p c) = foldl findbbox (p,p) c 
    where findbbox = \((Point(x1,y1),Point(x2,y2))) (Point(x3,y3)) -> 
                    ((Point(min x1 x3, min y1 y3), Point(max x2 x3, max y2 y3)))


width :: Curve -> Double
width c = xmax - xmin
        where (Point(xmin,_),Point(xmax,_)) = bbox c


height :: Curve -> Double
height c = ymax - ymin 
        where (Point(_,ymin),Point(_,ymax)) = bbox c


toList :: Curve -> [Point]
toList (Curve p c) = (p:c)


toSVG :: Curve -> String
toSVG c  = s 
       where s = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n"++
                 "width= \"" ++ printf "%d"(ceiling (width c) :: Int) ++ 
                 "px\" height= \"" ++ printf "%d" (ceiling (height c) :: Int) ++
                 "px\"  version= \"1.1\" >\n"++
                 "<g>\n"++
                 lineString c ++ 
                 "</g>\n"++
                 "</svg>"


lineString :: Curve -> String
lineString (Curve (Point(x1,y1)) (p@(Point(x2,y2)):cs)) = 
              "<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n"
              ++ x1s ++ x2s ++ y1s ++ y2s ++ " /> \n" ++ lineString (Curve p cs)
        where x1s = printf "x1=\"%.2f\" " x1 :: String
              x2s = printf "x2=\"%.2f\" " x2 :: String
              y1s = printf "y1=\"%.2f\" " y1 :: String
              y2s = printf "y2=\"%.2f\" " y2 :: String
lineString _ = ""


toFile :: Curve -> FilePath -> IO ()
toFile c fp = writeFile fp (toSVG c) 


hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c Horizontal 0

          c0 = ch `rotate` (-90) `translate` (point (w+p+w, h+p+h))
          c1 = c `translate` (point (w+p+w, h))
          c2 = c
          c3 = ch `rotate` 90 `translate` (point (0, h+p))
