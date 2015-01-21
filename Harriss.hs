module Harriss (Point(P), 
                Corner(C), LeftRight(Left, Right), UpperLower(Upper, Lower),             
                Rect(Rect), 
                Division(Div),
                divisions, 
                p, 
                generations,
                ArcOpening,
                Arc,
                DrawingInstruction,
                instructionsForDivision) where

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
      r1 = Rect (P x1 y1) (P x2 (y2 + h'))
      r2 = Rect (P x1 (y1 + h')) (P (x1 + w') y2)
-- something is going haywire in this div:
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

data ArcOpening = North | East | South | West deriving Show
data Arc = Arc {opening::ArcOpening, p1::Point, p2::Point} deriving Show

data DrawingInstruction = DI {entry1::Corner, sqr::Rect, arc1::Arc, arc2::Arc} deriving Show

instructionsForDivision :: Division -> DrawingInstruction
instructionsForDivision (Div c@(C Lower Left) s (r1, d1) (r2, d2)) = 
    DI c s (Arc West (lowerLeft s) ur) (Arc North ur (P ((x.lowerLeft.square) d2) (y ur))) 
        where 
          ur = (P ((x.lowerLeft) s) ((y.upperRight) s))
instructionsForDivision (Div c@(C Upper Left) s (r1, d1) (r2, d2)) = 
    DI c s (Arc North ul (upperRight s)) (Arc North (upperRight s) (P ((x.upperRight) s) ((y.lowerLeft.square) d2))) 
        where 
          ul = (P ((x.lowerLeft) s) ((y.upperRight) s))
instructionsForDivision (Div c@(C Upper Right) s (r1, d1) (r2, d2)) = 
    DI c s (Arc East (upperRight s) lr) (Arc South lr (P ((x.upperRight.square) d2) ((y.lowerLeft) s))) 
        where 
          lr = (P ((x.upperRight) s) ((y.lowerLeft) s))
instructionsForDivision (Div c@(C Lower Right) s (r1, d1) (r2, d2)) = 
    DI c s (Arc South lr (lowerLeft s)) (Arc West (lowerLeft s) (P ((x.upperRight.square) d2) ((y.lowerLeft) s))) 
        where 
          lr = (P ((x.upperRight) s) ((y.lowerLeft) s))


