{-# LANGUAGE RecordWildCards #-}

module MFAlgorithm where

import           AttributeGrammar
import           Control.Monad.State
import           Data.List
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Arc
import           AnalysisHelpers

--
--  * l : labels
--  * t : lattice
data MonotoneFramework t = MonotoneFramework
  { lattice        :: t
  , flow           :: [Arc Label]
  , blocks         :: [Block]
  , extremalLabels :: [Label]
  , extremalValue  :: t
  }

newtype Analysis_AE  = AE  (Set Expr  ) deriving (Eq, Ord, Show)
newtype Analysis_SLV = SLV (Set String) deriving (Eq, Ord, Show)

class Eq l => Lattice l where
  -- Het eerste argument is voor zodat je waardes in de bottom verzameling kan
  -- stoppen.
  bottom :: l -> l
  joinl  :: l -> l -> l

  joinls :: Foldable t => t l -> l
  joinls = foldr1 joinl

  before :: l -> l -> Bool
  before x y = x `joinl` y == x

instance Lattice Analysis_AE where
  bottom              = id -- Set AExp
  joinl (AE x) (AE y) = AE (S.intersection x y)

instance Lattice Analysis_SLV where
  bottom                = const $ SLV (S.empty) -- empty Set
  joinl (SLV x) (SLV y) = SLV (S.union x y)

-- Maximal Fixed Point

-- Type voor de iteratie
type IterationState t =
  State
    ( [Arc Label] -- Kantenstapel
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

      -- Overdrachtsfuncties volgens figuur 2.6 in NNH
      transfer :: Lattice t => Label -> Map Label t -> Map Label t
      transfer l current =
        (current `M.difference` (kill blocks l)) `M.union` gen blocks l

      -- Iteratie (Volgens het boek imperatief: dus met een state monad)
      -- w is de staat van de kantenstapel en a is de staat van de analyse. de
      -- analysis functie is een helper om het leesbaar te houden.
      --
      -- Wanneer w geen kanten meer bevat zijn we klaar. Anders is er nog een
      -- eerste kant die we moeten analyseren. Als de conditie (cond) /niet/
      -- waar is, wordt het binnenste gedeelte uitgevoerd:
      --
      --  * de staat van de analyse wordt aangepast
      --  * elke kant in de CFG die van de /volgende/ toestand vertrekt wordt op
      --    de kantenstapel gelegd.

      step :: Lattice t => IterationState t ()
      step =
        do
          (w, a) <- get
          let analysis l = fromJust $ M.lookup l a
          case w of
            []     -> return ()
            ((Intra from to):xs) -> -- TODO: Inter
              do
                let cond = (transfer from (analysis from)) `before` (analysis to)
                unless cond $ do
                  let a' = M.update (Just . joinl (transfer from (analysis from))) to a
                      w' = foldl' (flip (:)) xs [Intra to t
                                                | (Intra f t) <- flow
                                                , f == to]
                  put (w', a')
                step

      (_, mfp) = snd (runState step (reverse flow, analysis))
  in mfp

-- Testing

class Lattice t => KillGen t where
  kill :: [Block] -> Label -> t
  gen  :: [Block] -> Label -> t


instance KillGen Analysis_AE where
  gen b l =
    case lookup l b of
      Nothing -> error "label {} does not occur in the analysis"
      Just x ->
        case x of
          Right expr -> AE (S.singleton (B expr))
          Left  stat ->
            case stat of
              Skip'     _        -> AE (S.empty)
              IAssign'  _ _ expr -> AE (S.singleton (I expr))
              BAssign'  _ _ expr -> AE (S.singleton (B expr))
              Continue' _        -> AE (S.empty)
              Break'    _        -> AE (S.empty)

  kill b l =
    case lookup l b of
      Nothing -> error "label {} does not occur in the analysis"
      Just x ->
        case x of
          Right expr -> AE (S.empty)
          Left  stat ->
            case stat of
              Skip'     _        -> AE (S.empty)
              IAssign'  _ v expr ->
                AE (S.fromList [ e
                               | e <- expressions (I expr)
                               , v `notFreeIn` (I expr)])
              BAssign'  _ v expr ->
                AE (S.fromList [ e
                               | e <- expressions (B expr)
                               , v `notFreeIn` (B expr)])
              Continue' _        -> AE (S.empty)
              Break'    _        -> AE (S.empty)

testAE =
  MonotoneFramework
    (AE (S.fromList
          [ I (Plus  (Var "a") (Var "b"))
          , I (Times (Var "a") (Var "b"))
          , I (Plus  (Var "a") (IConst 1))
          ]))
    [Intra (Label 3) (Label 4), Intra (Label 7) (Label 8), Intra (Label 5) (Label 7), Intra (Label 8) (Label 5), Intra (Label 4) (Label 5)]
    []
    [(Label 3)]
    (AE (S.empty))

testSLV =
  MonotoneFramework
    (SLV (S.fromList
          ["a", "b", "x", "y"]))
    (fmap reverseArc [Intra (Label 3) (Label 4), Intra (Label 7) (Label 8), Intra (Label 5) (Label 7), Intra (Label 8) (Label 5), Intra (Label 4) (Label 5)])
    []
    [(Label 5)]
    (SLV (S.empty))

