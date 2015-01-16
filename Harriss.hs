module Harriss (Point, LeftRight, UpperLower, Corner, Rect, Division, p, divisions, generations) where

import Prelude hiding (Left, Right, length, div)

data Point = P Double Double deriving Show

data LeftRight = Left | Right
   deriving Show
data UpperLower = Upper | Lower
   deriving Show
data Corner = C UpperLower LeftRight
   deriving Show

data Rect = Rect {lowerLeft::Point, upperRight::Point} deriving Show

data Division = Div {entry::Corner, square::Rect, rect1::(Rect, Division), rect2::(Rect, Division)}

instance Show Division where
    show (Div c s (r1, _) (r2, _)) = "Div " ++ show c ++ " " ++ show s ++ " " ++ show r1 ++ " " ++ show r2

p :: Double
p = 1.32471795724474602596
                 
divisions :: Double -> Division
divisions h = div initRect (C Lower Left)
    where
      initRect = Rect (P 0 0) (P (h*p) h)

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
