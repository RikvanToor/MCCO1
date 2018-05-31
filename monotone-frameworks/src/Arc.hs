module Arc where

import Data.Function
import Data.List

data Arc a
  = Intra { from :: a, to :: a }
  | Inter { call :: a, entry :: a, exit :: a, return :: a }
  deriving Eq

instance Ord a => Ord (Arc a) where
  compare = on compare from

instance Show a => Show (Arc a) where
  show (Intra x y)     = show (x, y)
  show (Inter x y q w) = show (x, y) ++ ", " ++ show (q,w)

instance Functor Arc where
  fmap f (Intra x y)     = Intra (f x) (f y)
  fmap f (Inter x y q w) = Inter (f x) (f y) (f q) (f w)

reverseArc :: Arc a -> Arc a
reverseArc (Intra x y) = (Intra y x)
reverseArc (Inter x y q w) = (Inter y x w q) -- TODO

type Graph a = [Arc a]

reverseGraph :: Graph a -> Graph a
reverseGraph = fmap reverseArc

-- Alle knopen in de graaf
nodes :: Eq a => Graph a -> [a]
nodes =
  let f (Intra x y)     xs = (x:y:xs)
      f (Inter x y q w) xs = (x:y:q:w:xs)
  in nub . foldr f []

-- Inkomende en uitgaande verzamelingen
inbound :: Eq a => Graph a -> a -> [a]
inbound g on =
  let intra = [x | Intra x y     <- g, y == on]
      inter = [x | Inter x y _ _ <- g, y == on] -- TODO
  in intra ++ inter

outbound :: Eq a => Graph a -> a -> [a]
outbound g from_ =
  let intra = [x | Intra y x     <- g, y == from_]
      inter = [x | Inter y x _ _ <- g, y == from_] -- TODO
  in intra ++ inter

listArcsTowards :: Eq a => Graph a -> a -> [Arc a]
listArcsTowards g to =
  let intra = [Intra x y     | Intra x y     <- g, y == to]
      inter = [Inter x y q w | Inter x y q w <- g, y == to]
  in intra ++ inter

listArcsFrom :: Eq a => Graph a -> a -> [Arc a]
listArcsFrom g from =
  let intra = [Intra x y     | Intra x y     <- g, x == from]
      inter = [Inter x y q w | Inter x y q w <- g, x == from]
  in intra ++ inter

-- Predicaten over kanten
isIntra :: Arc a -> Bool
isIntra (Intra _ _) = True
isIntra _           = False

isInter :: Arc a -> Bool
isInter = not . isIntra
