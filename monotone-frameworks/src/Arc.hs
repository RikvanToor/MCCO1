module Arc
  ( Arc(..)
  , Graph
  , inbound
  , isInter
  , isIntra
  , nodes
  , outbound
  , reverseArc
  , reverseGraph
  ) where

import Data.List

data Arc a
  = Intra { from :: a, to :: a }
  | Inter { from :: a, to :: a, placeholder1 :: a, placeholder2 :: a }
  deriving Eq

instance Show a => Show (Arc a) where
  show (Intra x y)     = show (x, y)
  show (Inter x y q w) = undefined

instance Functor Arc where
  fmap f (Intra x y)     = Intra (f x) (f y)
  fmap f (Inter x y q w) = Inter (f x) (f y) (f q) (f w)

reverseArc :: Arc a -> Arc a
reverseArc (Intra x y) = (Intra y x)
reverseArc (Inter x y q w) = (Inter x y q w) -- TODO

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
outbound g from =
  let intra = [x | Intra y x     <- g, y == from]
      inter = [x | Inter y x _ _ <- g, y == from] -- TODO
  in intra ++ inter

-- Predicaten over kanten
isIntra :: Arc a -> Bool
isIntra (Intra _ _) = True
isIntra _           = False

isInter :: Arc a -> Bool
isInter = not . isIntra