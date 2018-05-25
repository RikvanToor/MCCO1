{-# LANGUAGE RecordWildCards #-}

module MFAlgorithm where

import           AttributeGrammar
import           Control.Monad.State
import           Data.List
import           Data.Map (Map)
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import           Arc

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

newtype Analysis_AE  = AE  (Set Expr  ) deriving (Eq, Ord, Show)
newtype Analysis_SLV = SLV (Set String) deriving (Eq, Ord, Show)

class Eq l => Lattice l where
  -- Het eerste argument is voor zodat je waardes in de bottom verzameling kan
  -- stoppen.
  bottom :: l -> l
  joinl   :: l -> l -> l

  before :: l -> l -> Bool
  before x y = x `joinl` y == x

instance Lattice Analysis_AE where
  bottom             = id -- Set AExp
  joinl (AE x) (AE y) = AE (S.intersection x y)

instance Lattice Analysis_SLV where
  bottom               = const $ SLV (S.empty) -- empty Set
  joinl (SLV x) (SLV y) = SLV (S.union x y)

-- Maximal Fixed Point

-- Type voor de iteratie
type IterationState t =
  State
    ( [Arc Label] -- Stack van kanten
    , Map Label t -- Huidige staat van de analysis
    )

maximalFixedPoint
  :: Lattice t
  => MonotoneFramework t -- Instantie van monotone framework
  -> Map Label t
maximalFixedPoint MonotoneFramework{..} =
  let {- Initialisatie (volgens het boek)
       - aan het einde van de initialisatie W = reverse flow en er wordt een map
       - aangemaakt waarin alle voor elke l in F of E: als l in E dan
       - analysis[l] = extremalValue, anders analysis[l] = bottom
       -}
      val v = if   v `elem` extremalLabels
              then extremalValue
              else bottom lattice
      analysis = M.fromList [(l, val l) | l <- nodes flow ++ extremalLabels]

      -- transfer :: Lattice t => Label -> Map Label t -> Map Label t
      transfer = id

      -- Iteration,
      step :: Lattice t => IterationState t ()
      step =
        do
          (w, a) <- get
          let analysis l = fromJust $ M.lookup l a
          case w of
            []     -> return ()
            ((Intra from to):xs) -> -- TODO: Inter
              do
                let cond = (transfer (analysis from)) `before` (analysis to)
                unless cond $ do
                  let a' = M.update (Just . joinl (transfer (analysis from))) to a
                      w' = foldl' (flip (:)) xs [Intra to t
                                                | (Intra f t) <- flow
                                                , f == to]
                  put (w', a')
                step

      (_, mfp) = snd (runState step (reverse flow, analysis))
  in mfp

-- Testing

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
