module Harriss (Point(P), 
                Corner(C), LeftRight(Left, Right), UpperLower(Upper, Lower),             
                Rect(Rect), 
                Division(Div),
                divisions, 
                p, 
                generations,
                Angle,
                MagnitudeMult,
                BCurve (BCurve),
                curvesForDivision) where

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

data BCurve = BCurve {p0::Point, p1::Point, p2::Point, p3::Point} deriving Show
type Angle = Double
type SinAndCosOfA = (Double, Double)
type MagnitudeMult = Double

curveForPointsVerticalUp :: SinAndCosOfA -> MagnitudeMult -> Point -> Point -> BCurve
curveForPointsVerticalUp (sa, ca) mm p0@(P x0 y0) p3@(P x3 y3) = BCurve p0 p1 p2 p3 
    where 
      d = y3 - y0
      l = mm*d
      l'= l/p --needed to get smoothness with next segment of curve
      p1 = P (x0 + l*sa) (y0 + l*ca)
      p2 = P (x3 + l'*ca) (y3 - l'*sa)
curveForPointsHorizontalRight :: SinAndCosOfA -> MagnitudeMult -> Point -> Point -> BCurve
curveForPointsHorizontalRight (sa, ca) mm p0@(P x0 y0) p3@(P x3 y3) = BCurve p0 p1 p2 p3 
    where 
      d = x0 - x3
      l = mm*d
      l'= l/p --needed to get smoothness with next segment of curve
      p1 = P (x0 - l*ca) (y0 + l*sa)
      p2 = P (x3 + l'*sa) (y3 + l'*ca)
curveForPointsVerticalDown :: SinAndCosOfA -> MagnitudeMult -> Point -> Point -> BCurve
curveForPointsVerticalDown (sa, ca) mm p0@(P x0 y0) p3@(P x3 y3) = BCurve p0 p1 p2 p3 
    where 
      d = y0 - y3
      l = mm*d
      l'= l/p --needed to get smoothness with next segment of curve
      p1 = P (x0 - l*sa) (y0 - l*ca)
      p2 = P (x3 - l'*ca) (y3 + l'*sa)
curveForPointsHorizontalLeft :: SinAndCosOfA -> MagnitudeMult -> Point -> Point -> BCurve
curveForPointsHorizontalLeft (sa, ca) mm p0@(P x0 y0) p3@(P x3 y3) = BCurve p0 p1 p2 p3 
    where 
      d = x3 - x0
      l = mm*d
      l'= l/p --needed to get smoothness with next segment of curve
      p1 = P (x0 + l*ca) (y0 - l*sa)
      p2 = P (x3 - l'*sa) (y3 - l'*ca)


curvesForDivision' :: SinAndCosOfA -> MagnitudeMult -> Division -> (BCurve, BCurve)
curvesForDivision' sc mm (Div (C Lower Left) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect (P x1' _) _) _ _)) = (a1, a2)
    where 
      p' = P x1 y2
      a1 = curveForPointsVerticalUp sc mm p1 p'
      a2 = curveForPointsHorizontalLeft sc mm p' (P x1' y2)
curvesForDivision' sc mm (Div (C Lower Right) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect (P _ y1') _) _ _)) = (a1, a2)
    where 
      p' = P x2 y1
      a1 = curveForPointsHorizontalRight sc mm p' p1
      a2 = curveForPointsVerticalUp sc mm p1 (P x1 y1')
curvesForDivision' sc mm (Div (C Upper Right) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect _ (P x2' _)) _ _)) = (a1, a2)
    where 
      p' = P x2 y1
      a1 = curveForPointsVerticalDown sc mm p2 p'
      a2 = curveForPointsHorizontalRight sc mm p' (P x2' y1)
curvesForDivision' sc mm (Div (C Upper Left) (Rect p1@(P x1 y1) p2@(P x2 y2)) _ (_, Div _ (Rect _ (P _ y2')) _ _)) = (a1, a2)
    where 
      p' = P x1 y2
      a1 = curveForPointsHorizontalLeft sc mm p' p2
      a2 = curveForPointsVerticalDown sc mm p2 (P x2 y2')

curvesForDivision :: Angle -> MagnitudeMult -> Division -> (BCurve, BCurve)
curvesForDivision a = curvesForDivision' (sin a, cos a)
