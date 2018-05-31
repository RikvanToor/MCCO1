{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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

-- MonotoneFramework datatype.

data MonotoneFramework t = MonotoneFramework
  { universe       :: t
  , flow           :: [Arc Label]
  , blocks         :: [Block]
  , extremalLabels :: [Label]
  , extremalValue  :: t
  }

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
 - Omdat er soms andere (arbitraire) operaties over verzamelingen gedaan moeten
 - worden over een lattice heeft het zin om een operatie @setOperation@ te
 - hebben die dat doet. daarvoor zijn ook weer de conversiefuncties toSet en
 - fromSet nodig.
 -
 -}
class Eq l => Lattice l where
  type Elem l

  bottom :: l -> l
  joinl  :: l -> l -> l

  before :: l -> l -> Bool
  before x y = x `joinl` y == y

  meet   :: l -> l -> Bool
  meet x y = x `joinl` y == x

  joinls :: Foldable t => t l -> l
  joinls = foldr1 joinl

  toSet   :: l -> Set (Elem l)
  fromSet :: Set (Elem l) -> l

  setOperation
    :: (Set (Elem l) -> Set (Elem l) -> Set (Elem l))
    -> l -> l -> l
  setOperation op l1 l2 =
    let set1 = toSet l1
        set2 = toSet l2
    in fromSet (set1 `op` set2)

-- Maximal Fixed Point

type LabelState t     = Map Label t
type IterationState t = State
  ( [Arc Label]
  , LabelState t
  )

maximalFixedPoint
  :: (KillGen t, Ord (Elem t), Show (t))
  => MonotoneFramework t -- Instantie van monotone framework
  -> LabelState t
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

      {- 2. Transfer functies
       -
       - In vrije set notatie doet deze f(x) = x \ kill(l) U gen(l)
       -
       -}
      transfer :: (KillGen t, Ord (Elem t)) => Label -> t -> t
      transfer l current =
        let killed =(kill blocks l)
            genned = (gen blocks l)
            killedResult = setOperation S.difference current killed
            gennedResult = setOperation S.union killedResult genned
        in  gennedResult


      {- 3. Iteratie
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
      step :: (KillGen t, Ord (Elem t), Show (t)) => IterationState t ()
      step =
        do
          (w, a) <- get
          let analysis l = a M.! l
          case w of
            []     -> return ()
            ((Intra from to):xs) -> -- TODO: Inter
              do
                let cond = (transfer from (analysis from)) `before` (analysis to)
                if cond then do
                  put (xs, a)
                else do
                  let a' = M.update (Just . joinl (transfer from (analysis from))) to a
                      w' = foldl' (flip (:)) xs (flow `listArcsFrom` to)

                  put (w', a')
                step
  in snd . snd . runState step $ (reverse flow, initial)

-- Kill & Gen sets
--
-- Deze zijn nodig om automatisch de transfer-functies te genereren. Voor elke
-- analyse moeten de kill & gen sets handmatig worden gedefinieerd.
class Lattice t => KillGen t where
  kill :: [Block] -> Label -> t
  gen  :: [Block] -> Label -> t


-- Analyse Available Expressions

newtype Analysis_AE  = AE  (Set Expr) deriving (Eq, Ord, Show)

instance Lattice Analysis_AE where
  type Elem Analysis_AE = Expr

  bottom              = id -- Set AExp
  joinl (AE x) (AE y) = AE (S.intersection x y)

  toSet (AE x)        = x
  fromSet             = AE

instance KillGen Analysis_AE where
  gen b l =
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

  kill b l =
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

analysis_AE :: Set Expr -> Flow -> [Block] -> [Label] -> Map Label Analysis_AE
analysis_AE universe flow blocks el =
  maximalFixedPoint $
    MonotoneFramework
      (AE universe)
      flow
      blocks
      el
      (AE S.empty)

-- Test voor tijdens het programmeren. TODO: Verwijder dit in de uiteindelijke
-- versie.

testAE =
    MonotoneFramework
      (AE (S.fromList [ I (Plus  (Var "a") (Var "b")) , I (Times (Var "a") (Var "b")) , I (Plus  (Var "a") (IConst 1)) ]))
      [Intra (Label 1) (Label 2), Intra (Label 4) (Label 5), Intra (Label 3) (Label 4), Intra (Label 5) (Label 3), Intra (Label 2) (Label 3)]
      (map (\(x,y) -> (Label x, y)) [(1,Left (IAssign' (Label 1) "x" (Plus (Var "a") (Var "b")))),(2,Left (IAssign' (Label 2) "y" (Times (Var "a") (Var "b")))),(3,Right (GreaterThan (Var "y") (Plus (Var "a") (Var "b")))),(4,Left (IAssign' (Label 4) "a" (Plus (Var "a") (IConst 1)))),(5,Left (IAssign' (Label 5) "x" (Plus (Var "a") (Var "b"))))])
      [(Label 1)]
      (AE S.empty)

-- Analyse Strongly Live Variables

newtype Analysis_SLV = SLV (Set String) deriving (Eq, Ord, Show)

instance Lattice Analysis_SLV where
  type Elem Analysis_SLV = String
  bottom                 = const $ SLV (S.empty) -- empty Set
  joinl (SLV x) (SLV y)  = SLV (S.union x y)
  toSet (SLV x)          = x
  fromSet                = SLV


instance KillGen Analysis_SLV where
  kill b l =
    case lookup l b of
      Nothing -> error $ "label " ++ show l ++ " does not occur in the analysis"
      Just x ->
        case x of
          Right expr -> fromSet S.empty
          Left  stat ->
            case stat of
              Skip'     _     -> fromSet S.empty
              IAssign'  _ v _ -> fromSet (S.singleton v)
              BAssign'  _ v _ -> fromSet (S.singleton v)
              Continue' _     -> fromSet (S.empty)
              Break'    _     -> fromSet (S.empty)

  gen  b l =
    case lookup l b of
      Nothing -> error $ "label " ++ show l ++ " does not occur in the analysis"
      Just x ->
        case x of
          Right expr -> fromSet . S.fromList . variables $ (B expr)
          Left  stat ->
            case stat of
              Skip'     _     -> fromSet S.empty
              IAssign'  _ _ a -> fromSet . S.fromList . variables $ (I a)
              BAssign'  _ _ a -> fromSet . S.fromList . variables $ (B a)
              Continue' _     -> fromSet (S.empty)
              Break'    _     -> fromSet (S.empty)

-- Test voor tijdens het programmeren. TODO: Verwijder dit in de uiteindelijke
-- versie.
testSLV =
  MonotoneFramework
    (SLV (S.fromList
          ["a", "b", "x", "y"]))
    (fmap reverseArc [Intra (Label 1) (Label 2), Intra (Label 4) (Label 5), Intra (Label 3) (Label 4), Intra (Label 5) (Label 3), Intra (Label 4) (Label 5)])
    (map (\(x,y) -> (Label x, y)) [(1,Left (IAssign' (Label 1) "x" (Plus (Var "a") (Var "b")))),(2,Left (IAssign' (Label 2) "y" (Times (Var "a") (Var "b")))),(3,Right (GreaterThan (Var "y") (Plus (Var "a") (Var "b")))),(4,Left (IAssign' (Label 4) "a" (Plus (Var "a") (IConst 1)))),(5,Left (IAssign' (Label 5) "x" (Plus (Var "a") (Var "b"))))])
    [(Label 3)]
    (SLV (S.empty))

-- Debugging printer: TODO REMOVE MAKE NICE
niceShow :: (Lattice l, Show (Elem l)) => Map k l -> IO ()
niceShow = mapM_ print . fmap (S.toList . toSet) . M.elems
