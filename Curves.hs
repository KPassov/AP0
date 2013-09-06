import Text.Printf

data Point = Point (Double, Double)
    deriving (Eq,Show,Read,Ord)
    
{- instance Eq Point where -}
	{- Point(x1,y1) == Point(x2,y2) = floor(x1*100)-floor(x2*100) == 0 && floor(y1*100)-floor(y2*100) == 0 -}
	{- Point(x1,y1) /= Point(x2,y2) = floor(x1*100)-floor(x2*100) /= 0 || floor(y1*100)-floor(y2*100) /= 0 -}
{- instance Eq Point where -}
    {- (==),(/=) :: a -> a -> Bool -}
    {- (Point (x1,y1)) == (Poin   -}


undef :: t
undef = undef

point:: (Double, Double) -> Point
point (a,b) = Point (a,b)

type Curve = [Point]

curve:: Point -> [Point] -> Curve
curve p x = p:x

connect :: Curve -> Curve -> Curve
connect c1 c2 = c1 ++ c2

rotate :: Curve -> Double -> Curve
rotate c degree = map rotate' c 
            where radiant = degree * (pi/180)
                  rotate' = (\(Point(x,y)) -> Point (x*cos(radiant) + y*sin(radiant),
                                                     y*cos(radiant) - x*sin(radiant)))

translate :: Curve -> Point -> Curve
translate [] p = [p] 
translate c@((Point(cx,cy)):_) (Point (px,py)) = map move c
            where move    = (\(Point (x1,x2)) -> Point (x1-dx,x2-dy))
                  (dx,dy) = (cx-px,cy-py)
                   
data Axis = Verticle | Horizontal

reflect :: Curve -> Axis -> Double -> Curve
reflect c axis d = map move c  
            where move = case axis  of
                           Verticle   -> (\(Point(x,y)) -> Point(x - (x-d)*2,y))
                           Horizontal -> (\(Point(x,y)) -> Point(x,y - (y-d)*2))

bbox :: Curve -> (Point, Point)
bbox [] = (Point(0,0),Point(0,0))
bbox (c:cs) = foldl findbbox (c,c) cs
	where findbbox = \((Point(minx,miny),Point(maxx,maxy))) (Point(x,y)) -> 
					 ((Point(min minx x, min miny y), Point(max maxx x, max maxy y)))

width :: Curve -> Double
width c = xmax - xmin
        where (Point(xmin,_),Point(xmax,_)) = bbox c

height :: Curve -> Double
height c = ymax - ymin 
        where (Point(_,ymin),Point(_,ymax)) = bbox c

toList :: Curve -> [Point]
toList c = c  

toSVG :: Curve -> String
toSVG c  = s 
        where s = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n"++
                  "width= \"" ++ show (ceiling (width c) :: Int) ++ "px\" height= \"" ++ show (ceiling (height c) :: Int) ++ "px\"  version= \"1.1\" >\n"++
                  "<g>\n"++
                  lineString c ++ 
                  "</g>\n"++
                  "</svg>"

lineString :: Curve -> String
lineString ((Point(x1,y1)):cs@(Point(x2,y2)):css) = s ++ x1s ++ x2s ++ y1s ++ y2s ++ " /> \n"
        where s = "<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n"
            x1s = printf "x1=\"%.2f\" " x1 :: String
            x2s = printf "x2=\"%.2f\" " x2 :: String
            y1s = printf "y1=\"%.2f\" " y1 :: String
            y2s = printf "y2=\"%.2f\" " y2 :: String
lineString _ = ""


toFile :: Curve -> FilePath -> IO ()
toFile c f = writeFile f (toSVG c) 

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

{- a = [Point(3.1,3.2),Point(3.3,3.5)]  -}
{- b = [Point(4.2,5.1),Point(7.0,3.5)]  -}

