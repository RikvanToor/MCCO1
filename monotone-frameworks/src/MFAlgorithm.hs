{-# LANGUAGE RecordWildCards #-}

module MFAlgorithm where

import           AttributeGrammar
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.List
import qualified Data.Map as M
import qualified Data.Set as S

--
--  * l : labels
--  * t : lattice
data MonotoneFramework t = MonotoneFramework
  { lattice        :: t
  , flow           :: [Arc Label]
  , extremalLabels :: [Label]
  , extremalValue  :: t
  } deriving Eq

instance Show t => Show (MonotoneFramework t) where
  show = undefined

newtype Analysis_AE  = AE  { lattice_ae  :: Set Expr   } deriving (Eq, Ord, Show)
newtype Analysis_SLV = SLV { lattice_slv :: Set String } deriving (Eq, Ord, Show)

class Eq l => Lattice l where
  -- Het eerste argument is voor zodat je waardes in de bottom verzameling kan
  -- stoppen.
  bottom :: l -> l
  join   :: l -> l -> l

  before :: l -> l -> Bool
  before x y = x `join` y == x

instance Lattice Analysis_AE where
  bottom             = id -- Set AExp
  join (AE x) (AE y) = AE (S.intersection x y)

instance Lattice Analysis_SLV where
  bottom               = const $ SLV (S.empty) -- empty Set
  join (SLV x) (SLV y) = SLV (S.union x y)

-- Maximal Fixed Point

mfpinit
  :: Lattice t
  => MonotoneFramework t -- Instantie van monotone framework
  -> Map Label t
mfpinit MonotoneFramework{..}  =
  let val v = if   v `elem` extremalLabels
              then extremalValue
              else bottom lattice
      -- later veranderen voor Inter
      labels_unique = sort . nub . concat $ [[x,y] | (Intra x y) <- flow]
      initial = M.fromList [(l, val l) | l <- labels_unique]
  in initial

testAE =
  MonotoneFramework
    (AE (S.fromList
          [ I (Plus  (Var "a") (Var "b"))
          , I (Times (Var "a") (Var "b"))
          , I (Plus  (Var "a") (IConst 1))
          ]))
    [Intra (Label 3) (Label 4), Intra (Label 7) (Label 8), Intra (Label 5) (Label 7), Intra (Label 8) (Label 5), Intra (Label 4) (Label 5)]
    [(Label 3)]
    (AE (S.empty))

testSLV =
  MonotoneFramework
    (SLV (S.fromList
          ["a", "b", "x", "y"]))
    (fmap reverseArc [Intra (Label 3) (Label 4), Intra (Label 7) (Label 8), Intra (Label 5) (Label 7), Intra (Label 8) (Label 5), Intra (Label 4) (Label 5)])
    [(Label 5)]
    (SLV (S.empty))

