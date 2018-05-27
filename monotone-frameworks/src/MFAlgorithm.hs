{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Debug.Trace -- Remove

--
--  * l : labels
--  * t : lattice
data MonotoneFramework t = MonotoneFramework
  { universe       :: t
  , flow           :: [Arc Label]
  , blocks         :: [Block]
  , extremalLabels :: [Label]
  , extremalValue  :: t
  }

newtype Analysis_AE  = AE  (Set Expr  ) deriving (Eq, Ord, Show)
newtype Analysis_SLV = SLV (Set String) deriving (Eq, Ord, Show)

{- Lattice type class
 -
 -}
class Eq l => Lattice l where
  type Elem l

  -- Het eerste argument is voor zodat je waardes in de bottom verzameling kan
  -- stoppen.
  bottom :: l -> l
  joinl  :: l -> l -> l

  before :: l -> l -> Bool
  before x y = x `joinl` y == x

  -- Lijst versie van join semi-lattice
  joinls :: Foldable t => t l -> l
  joinls = foldr1 joinl

  -- Bewerkingen over verzamelingen vereisen een manier om van tralie naar
  -- verzameling te gaan
  toSet   :: l -> Set (Elem l)
  fromSet :: Set (Elem l) -> l

  setOperation
    :: (Set (Elem l) -> Set (Elem l) -> Set (Elem l))
    -> l -> l -> l
  setOperation op l1 l2 =
    let set1 = toSet l1
        set2 = toSet l2
    in fromSet (set1 `op` set2)

instance Lattice Analysis_AE where
  type Elem Analysis_AE = Expr

  bottom              = id -- Set AExp
  joinl (AE x) (AE y) = AE (S.intersection x y)

  toSet (AE x)        = x
  fromSet             = AE


instance Lattice Analysis_SLV where
  type Elem Analysis_SLV = String
  bottom                 = const $ SLV (S.empty) -- empty Set
  joinl (SLV x) (SLV y)  = SLV (S.union x y)
  toSet (SLV x)          = x
  fromSet                = SLV

-- Maximal Fixed Point

-- Type voor de iteratie
type IterationState t =
  State
    ( [Arc Label] -- Kantenstapel
    , Map Label t -- Huidige staat van de analysis
    )

maximalFixedPoint
  :: (KillGen t, Ord (Elem t), Show (t))
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
              else bottom universe -- universe wordt hier geinjecteerd voor het geval dat bottom niet de lege verzameling is
      initial = M.fromList [(l, val l) | l <- nodes flow ++ extremalLabels]

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

      step :: (KillGen t, Ord (Elem t), Show (t)) => IterationState t ()
      step =
        let
          -- Overdrachtsfuncties volgens figuur 2.6 in NNH
          transfer l current =
            let diff = setOperation S.difference current (kill blocks l)
            in setOperation S.union diff (gen blocks l)
        in do
          (w, a) <- get
          let analysis l = fromJust $ M.lookup l a
          case w of
            []     -> return ()
            ((Intra from to):xs) -> -- TODO: Inter
              do
                let cond = (transfer from (analysis from)) `before` (analysis to)
                unless cond $ do
                  let a' = M.update (Just . joinl (transfer from (analysis from))) to a
                      w' = foldl' (flip (:)) xs (xs `listArcsFrom` to)

                  put (w', a')
                step

      (_, mfp) = snd (runState step (reverse flow, initial))
  in snd . snd . runState step $ (reverse flow, initial)

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
    (map (\(x,y) -> (Label x, y)) [(3,Left (IAssign' (Label 3) "x" (Plus (Var "a") (Var "b")))),(4,Left (IAssign' (Label 4) "y" (Times (Var "a") (Var "b")))),(5,Right (GreaterThan (Var "y") (Plus (Var "a") (Var "b")))),(7,Left (IAssign' (Label 7) "a" (Plus (Var "a") (IConst 1)))),(8,Left (IAssign' (Label 8) "x" (Plus (Var "a") (Var "b"))))])
    [(Label 3)]
    (AE (S.empty))

testSLV =
  MonotoneFramework
    (SLV (S.fromList
          ["a", "b", "x", "y"]))
    (fmap reverseArc [Intra (Label 3) (Label 4), Intra (Label 7) (Label 8), Intra (Label 5) (Label 7), Intra (Label 8) (Label 5), Intra (Label 4) (Label 5)])
    (map (\(x,y) -> (Label x, y)) [(3,Left (IAssign' (Label 3) "x" (Plus (Var "a") (Var "b")))),(4,Left (IAssign' (Label 4) "y" (Times (Var "a") (Var "b")))),(5,Right (GreaterThan (Var "y") (Plus (Var "a") (Var "b")))),(7,Left (IAssign' (Label 7) "a" (Plus (Var "a") (IConst 1)))),(8,Left (IAssign' (Label 8) "x" (Plus (Var "a") (Var "b"))))])
    [(Label 5)]
    (SLV (S.empty))

