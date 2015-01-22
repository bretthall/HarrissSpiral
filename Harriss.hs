module Harriss (Point(P), 
                Corner(C), LeftRight(Left, Right), UpperLower(Upper, Lower),             
                Rect(Rect), 
                Division(Div),
                divisions, 
                p, 
                generations,
                Arc (Arc),
                arcsForDivision) where

import Prelude hiding (Left, Right, length, div)

data Point = P {x::Double, y::Double} deriving Show

data LeftRight = Left | Right deriving Show
data UpperLower = Upper | Lower deriving Show
data Corner = C UpperLower LeftRight deriving Show

data Rect = Rect {lowerLeft::Point, upperRight::Point} deriving Show

data Division = Div {entry::Corner, square::Rect, rect1::(Rect, Division), rect2::(Rect, Division)}

instance Show Division where
    show (Div c s (r1, _) (r2, _)) = "Div " ++ show c ++ " " ++ show s ++ " " ++ show r1 ++ " " ++ show r2

p :: Double
p = 1.32471795724474602596
                 
divisions :: Double -> Division
divisions w = div initRect (C Lower Left)
    where
      initRect = Rect (P 0 0) (P w (w/p))

div :: Rect -> Corner -> Division
div (Rect (P x1 y1) (P x2 y2)) c@(C Lower Left) = Div c sqr (r1, div r1 (C Lower Right)) (r2, div r2 c)
    where
      h = y2 - y1
      w = x2 - x1
      h' = h / (1.0 + p)
      s = h' * p
      w' = h / p
      sqr = Rect (P (x1 + w') y1) (P x2 (y1 + s)) 
      r1 = Rect (P x1 y1) (P (x1 + w') y2)
      r2 = Rect (P (x1 + w') (y1 + s)) (P x2 y2)
div (Rect (P x1 y1) (P x2 y2)) c@(C Lower Right) = Div c sqr (r1, div r1 (C Upper Right)) (r2, div r2 c)
    where
      h = y2 - y1
      w = x2 - x1
      w' = w / (1.0 + p)
      s = w' * p
      h' = w / p
      sqr = Rect (P (x1 + w') (y1 + h')) (P x2 y2) 
      r1 = Rect (P x1 y1) (P x2 (y1 + h'))
      r2 = Rect (P x1 (y1 + h')) (P (x1 + w') y2)
div (Rect (P x1 y1) (P x2 y2)) c@(C Upper Right) = Div c sqr (r1, div r1 (C Upper Left)) (r2, div r2 c)
    where
      h = y2 - y1
      w = x2 - x1
      h' = h / (1.0 + p)
      s = h' * p
      w' = h / p
      sqr = Rect (P x1 (y1 + h')) (P (x1 + s) y2)
      r1 = Rect (P (x1 + s) y1) (P x2 y2)
      r2 = Rect (P x1 y1) (P (x1 + s) (y1 + h'))
div (Rect (P x1 y1) (P x2 y2)) c@(C Upper Left) = Div c sqr (r1, div r1 (C Lower Left)) (r2, div r2 c)
    where
      h = y2 - y1
      w = x2 - x1
      w' = w / (1.0 + p)
      s = w' * p
      h' = w / p
      sqr = Rect (P x1 y1) (P (x1 + s) (y1 + s)) 
      r1 = Rect (P x1 (y1 + s)) (P x2 y2)
      r2 = Rect (P (x1 + s) y1) (P x2 (y1 + s))

generations :: Division -> [[Division]]
generations d = [d]:(next [d])
    where
      next ds = ds':(next ds')
          where 
            ds' = concatMap (\d -> [(snd.rect1) d, (snd.rect2) d]) ds

data Arc = Arc {center::Point, radius::Double, angle1::Double, angle2::Double} deriving Show

radiusMult = 1.5

arcForPointsVerticalUp :: Point -> Point -> Arc
arcForPointsVerticalUp (P x1 y1) (P x2 y2) = Arc c r a1 a2
    where 
      d = y2 - y1
      r = radiusMult * d
      a = asin (d/(2.0*r))
      a1 = -a
      a2 = a
      c = P (x1 - r*(sin a)) (y1 + d/2.0)
arcForPointsVerticalDown :: Point -> Point -> Arc
arcForPointsVerticalDown (P x1 y1) (P x2 y2) = Arc c r a1 a2
    where 
      d = y1 - y2
      r = radiusMult * d
      a = asin (d/(2.0*r))
      a1 = pi - a
      a2 = pi + a
      c = P (x1 + r*(sin a)) (y2 + d/2.0)
arcForPointsHorizontalLeft :: Point -> Point -> Arc
arcForPointsHorizontalLeft (P x1 y1) (P x2 y2) = Arc c r a1 a2
    where 
      d = x2 - x1
      r = radiusMult * d
      a = asin (d/(2.0*r))
      a1 = 3.0*pi/2.0 - a
      a2 = 3.0*pi/2.0 + a
      c = P (x1 + d/2.0) (y1 + r*(sin a))
arcForPointsHorizontalRight :: Point -> Point -> Arc
arcForPointsHorizontalRight (P x1 y1) (P x2 y2) = Arc c r a1 a2
    where 
      d = x1 - x2
      r = radiusMult * d
      a = asin (d/(2.0*r))
      a1 = pi/2.0 - a
      a2 = pi/2.0 + a
      c = P (x1 + d/2.0) (y1 - r*(sin a))

arcsForDivision :: Division -> (Arc, Arc)
arcsForDivision (Div (C Lower Left) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect (P x1' _) _) _ _)) = (a1, a2)
    where 
      p' = P x1 y2
      a1 = arcForPointsVerticalUp p1 p'
      a2 = arcForPointsHorizontalLeft p' (P x1' y2)
arcsForDivision (Div (C Lower Right) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect _ (P _ y2')) _ _)) = (a1, a2)
    where 
      p' = P x2 y1
      a1 = arcForPointsHorizontalRight p' p1
      a2 = arcForPointsVerticalUp p1 (P x1 y2')
arcsForDivision (Div (C Upper Right) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect _ (P x2' _)) _ _)) = (a1, a2)
    where 
      p' = P x2 y1
      a1 = arcForPointsVerticalDown p2 p'
      a2 = arcForPointsHorizontalRight p' (P x2' y1)
arcsForDivision (Div (C Upper Left) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect _ (P _ y2')) _ _)) = (a1, a2)
    where 
      p' = P x1 y2
      a1 = arcForPointsHorizontalLeft p' p2
      a2 = arcForPointsVerticalDown p2 (P x2 y2')


