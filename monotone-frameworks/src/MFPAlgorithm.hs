{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module MFPAlgorithm where

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

import Debug.Trace -- TODO: Remove

type LabelState l     = Map Label l

-- MonotoneFramework datatype.

data MonotoneFramework l = MonotoneFramework
  { universe       :: l
  , flow           :: [Arc Label]
  , blocks         :: [Block]
  , extremalLabels :: [Label]
  , extremalValue  :: l
  , transferFuns   :: LabelState (l -> l)
  }

mkMFInstance
  :: Transferable l -- Tralies waarover transfer functies zijn gedefinieerd
  => l -> Flow -> [Block] -> [Label] -> l -> MonotoneFramework l
mkMFInstance universe flow blocks extremalLabels extremalValue =
  MonotoneFramework
    universe
    flow
    blocks
    extremalLabels
    extremalValue
    (transfers blocks)

{- Lattice type class
 -
 - De fundamentele eigenschappen van deze type class zijn bottom en joinl.
 - Bottom zou eigenlijk als type l moetem hebben, maar het /universum/ van bijv.
 - alle expressies moet op een of andere manier geinjecteerd worden. joinl staat
 - voor join-lattice en is de join operator. Vaak zal dit een gelifte versie van
 - een set-operatie zijn.
 -
 - before (van comes-before) is volledig geimplementeerd in termen van bottom en
 - joinl.
 -
 -}
class Eq l => Lattice l where
  bottom :: l -> l
  joinl  :: l -> l -> l

  before :: l -> l -> Bool
  before x y = x `joinl` y == y

-- Sommige lattices hebben een verzameling representatie, dan kunnen we fijn
-- direct op
class Lattice l => SetRepr l where
  type Elem l
  toSet   :: l -> Set (Elem l)

-- Maximal Fixed Point

type IterationState l = State
  ( [Arc Label]
  , LabelState l
  )

maximalFixedPoint
  :: forall l. Transferable l
  => MonotoneFramework l
  -> LabelState l
maximalFixedPoint MonotoneFramework{..} =
  let {- 1. Initialisatie
       -
       - Aan het einde van de initialisatie W = reverse flow en er wordt een map
       - aangemaakt waarin alle voor elke l in F of E: als l in E dan
       - analysis[l] = extremalValue, anders analysis[l] = bottom.
       -
       - Let op: hier (*) wordt de waarde van bottom geinjecteerd
       -}
      val v = if   v `elem` extremalLabels
              then extremalValue
              else bottom universe -- (*)
      initial = M.fromList [(l, val l) | l <- nodes flow ++ extremalLabels]


      {- 2. Iteratie
       -
       - w is de staat van de kantenstapel en a is de staat van de analyse. de
       - analysis functie is een helper om het leesbaar te houden.
       -
       - Wanneer w geen kanten meer bevat zijn we klaar. Anders is er nog een
       - eerste kant die we moeten analyseren. Als de conditie (cond) /niet/
       - waar is, wordt het binnenste gedeelte uitgevoerd:
       -
       -  * de staat van de analyse wordt aangepast
       -  * elke kant in de CFG die van de /volgende/ toestand vertrekt wordt op
       -    de kantenstapel gelegd.
       -}
      step :: IterationState l ()
      step =
        do
          (w, a) <- get
          let analysis l = a M.! l
          case w of
            []     -> Control.Monad.State.return ()
            ((Intra from to):xs) -> -- TODO: Inter
              do
                let next = (transferFuns M.! from) (analysis from)
                    cond = next `before` (analysis to)
                if cond then do
                  put (xs, a)
                else do
                  let a' = M.update (Just . joinl next) to a
                      w' = foldl' (flip (:)) xs (flow `listArcsFrom` to)

                  put (w', a')
                step
  in snd . snd . runState step $ (reverse flow, initial)

-- Class voor het genereren van transfer functies
class Lattice l => Transferable l where
  transfers :: [Block] -> Map Label (l -> l)

-- Available expressions

newtype Analysis_AE  = AE { unAE :: Set Expr } deriving (Eq, Ord, Show)

instance Lattice Analysis_AE where
  bottom              = id -- Set AExp
  joinl (AE x) (AE y) = AE (S.intersection x y)

instance SetRepr Analysis_AE where
  type Elem Analysis_AE = Expr
  toSet   = unAE

instance Transferable Analysis_AE where
  transfers b =
    let kill l =
          let aexp_star = concatMap (either statExpressions (expressions . B) . snd) b
          in case lookup l b of
            Nothing -> error $ "label " ++ show l ++ " does not occur in the analysis"
            Just x ->
              case x of
                Right expr -> AE (S.empty)
                Left  stat ->
                  case stat of
                    Skip'     _        -> AE (S.empty)
                    IAssign'  _ v expr ->
                      AE . S.fromList $ filter (notFreeIn v) aexp_star
                    BAssign'  _ v expr ->
                      AE . S.fromList $ filter (notFreeIn v) aexp_star
                    Continue' _        -> AE (S.empty)
                    Break'    _        -> AE (S.empty)

        gen l =
          case lookup l b of
            Nothing -> error $ "label " ++ show l ++ " does not occur in the analysis"
            Just x ->
              case x of
                Right expr -> AE (S.singleton (B expr))
                Left  stat ->
                  case stat of
                    Skip'     _        -> AE (S.empty)
                    IAssign'  _ v expr ->
                      AE . S.fromList $ filter (freeIn v) (expressions (I expr))
                    BAssign'  _ v expr ->
                      AE . S.fromList $ filter (freeIn v) (expressions (B expr))
                    Continue' _        -> AE (S.empty)
                    Break'    _        -> AE (S.empty)

        t l = \(AE t) ->
          let (AE killSet) = kill l
              (AE genSet)  = gen  l
          in AE $ (t `S.difference` killSet) `S.union` genSet

    in M.fromList [(l, t l) | l <- fmap fst b]

-- Test voor tijdens het programmeren. TODO: Verwijder dit in de uiteindelijke
-- versie.

testAE =
    mkMFInstance
      (AE (S.fromList [ I (Plus  (Var "a") (Var "b")) , I (Times (Var "a") (Var "b")) , I (Plus  (Var "a") (IConst 1)) ]))
      [Intra (Label 1) (Label 2), Intra (Label 4) (Label 5), Intra (Label 3) (Label 4), Intra (Label 5) (Label 3), Intra (Label 2) (Label 3)]
      (map (\(x,y) -> (Label x, y)) [(1,Left (IAssign' (Label 1) "x" (Plus (Var "a") (Var "b")))),(2,Left (IAssign' (Label 2) "y" (Times (Var "a") (Var "b")))),(3,Right (GreaterThan (Var "y") (Plus (Var "a") (Var "b")))),(4,Left (IAssign' (Label 4) "a" (Plus (Var "a") (IConst 1)))),(5,Left (IAssign' (Label 5) "x" (Plus (Var "a") (Var "b"))))])
      [(Label 1)]
      (AE S.empty)

-- Analyse Strongly Live Variables

newtype Analysis_SLV = SLV { unSLV :: Set String } deriving (Eq, Ord, Show)

instance Lattice Analysis_SLV where
  bottom                 = const (SLV S.empty)
  joinl (SLV x) (SLV y)  = SLV (S.union x y)

instance SetRepr Analysis_SLV where
  type Elem Analysis_SLV = String
  toSet   = unSLV

instance Transferable Analysis_SLV where
  transfers b =
    let kill l =
          case lookup l b of
            Nothing -> error $ "label " ++ show l ++ " does not occur in the analysis"
            Just x ->
              case x of
                Right expr -> SLV S.empty
                Left  stat ->
                  case stat of
                    Skip'     _     -> SLV S.empty
                    IAssign'  _ v _ -> SLV (S.singleton v)
                    BAssign'  _ v _ -> SLV (S.singleton v)
                    Continue' _     -> SLV (S.empty)
                    Break'    _     -> SLV (S.empty)

        gen l =
          case lookup l b of
            Nothing -> error $ "label " ++ show l ++ " does not occur in the analysis"
            Just x ->
              case x of
                Right expr -> SLV . S.fromList . variables $ (B expr)
                Left  stat ->
                  case stat of
                    Skip'     _     -> SLV S.empty
                    IAssign'  _ _ a -> SLV . S.fromList . variables $ (I a)
                    BAssign'  _ _ a -> SLV . S.fromList . variables $ (B a)
                    Continue' _     -> SLV (S.empty)
                    Break'    _     -> SLV (S.empty)
        t l = \(SLV t) ->
          let (SLV killSet) = kill l
              (SLV genSet)  = gen  l
          in SLV $ (t `S.difference` killSet) `S.union` genSet

    in M.fromList [(l, t l) | (l, _) <- b]

-- Test voor tijdens het programmeren. TODO: Verwijder dit in de uiteindelijke
-- versie.
testSLV =
  mkMFInstance
    (SLV (S.fromList
          ["a", "b", "x", "y"]))
    (fmap reverseArc [Intra (Label 1) (Label 2), Intra (Label 4) (Label 5), Intra (Label 3) (Label 4), Intra (Label 5) (Label 3), Intra (Label 4) (Label 5)])
    (map (\(x,y) -> (Label x, y)) [(1,Left (IAssign' (Label 1) "x" (Plus (Var "a") (Var "b")))),(2,Left (IAssign' (Label 2) "y" (Times (Var "a") (Var "b")))),(3,Right (GreaterThan (Var "y") (Plus (Var "a") (Var "b")))),(4,Left (IAssign' (Label 4) "a" (Plus (Var "a") (IConst 1)))),(5,Left (IAssign' (Label 5) "x" (Plus (Var "a") (Var "b"))))])
    [(Label 3)]
    (SLV (S.empty))

-- Debugging printer: TODO REMOVE MAKE NICE
niceShow :: (SetRepr l, Show (Elem l)) => Map k l -> IO ()
niceShow = mapM_ print . fmap (S.toList . toSet) . M.elems
