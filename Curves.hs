data Point = Point (Double, Double)
    deriving (Eq, Show,Read,Ord)
{- instance Eq Point where -}
    {- (==),(/=) :: a -> a -> Bool -}
    {- (Point (x1,y1)) == (Poin   -}

{- instance Num Point where -}
    {- Point(x1,x2) + Point(y1,y2) = Point(x1+y1,x2+y2)  -}
    {- Point(x1,x2) - Point(y1,y2) = Point(x1-y1,x2-y2)  -}
    {- (*) = undef     -}
    {- abs = undef     -}
    {- signum = undef     -}
    {- fromInteger = undef     -}

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
height = ymax - xmin 
        where (Point(_,ymin),Point(_,ymax)) = bbox c

toList :: Curve -> [Point]
toList c = c  

toSVG :: Curve -> String
toSVG _ = undef

toFile :: Curve -> FilePath -> IO ()
toFile _ _ = undef

{- a = [Point(3.1,3.2),Point(3.3,3.5)]  -}
{- b = [Point(4.2,5.1),Point(7.0,3.5)]  -}

