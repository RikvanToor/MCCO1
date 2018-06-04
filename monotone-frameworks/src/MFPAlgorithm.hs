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

import Debug.Trace -- TODO: Remove

type LabelState l     = Map Label l

-- MonotoneFramework datatype.

data MonotoneFramework l = MonotoneFramework
  { universe       :: l
  , flow           :: [Arc Label]
  , blocks         :: [Block]
  , extremalLabels :: [Label]
  , extremalValue  :: l
  , transferFuns   :: LabelState ([Label] -> l -> l)
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
  toSet :: l -> Set (Elem l)

{- Maximal Fixed Point
 -
 - Levert de Analysis-open op. Als de transfer functies nogmaals worden
 - toegepast op het resultaat van maximalFixedPoint dan krijg je de
 - Analysis-gesloten.
 -}

type IterationState l = State ([Arc Label], LabelState l, [Label])

maximalFixedPoint
  :: forall l. Transferable l
  => Int
  -> MonotoneFramework l
  -> (LabelState l, LabelState l)
maximalFixedPoint k MonotoneFramework{..} =
  let
    {- 1. Initialisatie
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
        (w, a, ctx) <- get
        let analysis l = a M.! l
        case w of
          []     -> Control.Monad.State.return ()
          ((Inter call entry exit return):xs) -> 
            do          
              let nctx = take k $ call:ctx
                  next = (transferFuns M.! call) nctx (analysis call)
                  cond = next `before` (analysis entry)
              if cond then do
                put (xs, a, nctx)
              else do
                let a' = M.update (\x -> Just (x `joinl` next)) entry a 
                    w' = foldl' (flip (:)) xs (listArcsFrom flow entry nctx)

                put (w', a', nctx)
              step
          ((Intra from to):xs) -> -- TODO: Inter
            do
              let next = (transferFuns M.! from) ctx (analysis from)
                  cond = next `before` (analysis to)
              if cond then do
                put (xs, a, ctx)
              else do
                let a' = M.update (\x -> Just (x `joinl` next)) to a
                    w' = foldl' (flip (:)) xs (listArcsFrom flow to ctx)

                put (w', a', ctx)
              step

    result = snd . runState step $ (reverse flow, initial, [])
    -- MFP-open
    contextState = snd3 result

    callStrings = trd3 result

    -- MFP-closed
    effectState  = M.mapWithKey (\l s -> (transferFuns M.! l) callStrings s) contextState

    -- Voor nu nog even handmatig wisselen tussen open en closed.
  in (contextState, effectState)

-- Class voor het genereren van transfer functies
class Lattice l => Transferable l where
  transfers :: [Block] -> Map Label ([Label] -> l -> l)

-- Available expressions

newtype Analysis_AE  = AE { unAE :: Set Expr } deriving (Eq, Ord, Show)

instance Lattice Analysis_AE where
  bottom              = id -- Set AExp
  joinl (AE x) (AE y) = AE (S.intersection x y)

instance SetRepr Analysis_AE where
  type Elem Analysis_AE = Expr
  toSet = unAE

instance Transferable Analysis_AE where
  transfers b =
    let kill ctx l =
          let
            aexp_star =
              let f (Boolean   expr) = expressions (B expr)
                  f (Statement stat) = statExpressions stat
                  f _                = []
              in concatMap (f . snd) b
          in case lookup l b of
            Nothing -> analysisLookupError l
            Just x ->
              case x of
                Boolean   expr -> AE (S.empty)
                Statement stat ->
                  case stat of
                    Skip'     _        -> AE (S.empty)
                    IAssign'  _ v expr ->
                      AE . S.fromList $ filter (notFreeIn v) aexp_star
                    BAssign'  _ v expr ->
                      AE . S.fromList $ filter (notFreeIn v) aexp_star

        gen ctx l =
          case lookup l b of
            Nothing -> analysisLookupError l
            Just x ->
              case x of
                Boolean expr   -> AE (S.singleton (B expr))
                Statement stat ->
                  case stat of
                    Skip'     _        -> AE (S.empty)
                    IAssign'  _ v expr ->
                      AE . S.fromList $ filter (freeIn v) (expressions (I expr))
                    BAssign'  _ v expr ->
                      AE . S.fromList $ filter (freeIn v) (expressions (B expr))
                IsProc     pb -> AE (S.empty)
                EndProc    pb -> AE (S.empty)
                CallProc   cb -> AE (S.empty)
                ReturnProc cb -> AE (S.empty)

        t l = \ctx (AE t) ->
          let (AE killSet) = kill ctx l
              (AE genSet)  = gen  ctx l
          in AE $ (t S.\\ killSet) `S.union` genSet
    in M.fromList [(l, t l) | (l, _) <- b]

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
    let
      kill ctx l =
        case lookup l b of
          Nothing -> analysisLookupError l
          Just x ->
            case x of
              Boolean expr   -> SLV S.empty
              Statement stat ->
                case stat of
                  Skip'     _     -> SLV S.empty
                  IAssign'  _ v _ -> SLV (S.singleton v)
                  BAssign'  _ v _ -> SLV (S.singleton v)
                  Continue' _     -> SLV S.empty
                  Break'    _     -> SLV S.empty
              IsProc      pb -> SLV S.empty
              EndProc     pb -> SLV S.empty
              CallProc    cb -> SLV S.empty
              ReturnProc  cb -> SLV S.empty

      gen ctx l =
        case lookup l b of
          Nothing -> analysisLookupError l
          Just x ->
            case x of
              Boolean   expr -> SLV . S.fromList . variables $ (B expr)
              Statement stat ->
                case stat of
                  Skip'     _     -> SLV S.empty
                  IAssign'  _ _ a -> SLV . S.fromList . variables $ (I a)
                  BAssign'  _ _ a -> SLV . S.fromList . variables $ (B a)
                  Continue' _     -> SLV S.empty
                  Break'    _     -> SLV S.empty
              IsProc      pb -> SLV S.empty
              EndProc     pb -> SLV S.empty
              CallProc    cb -> SLV S.empty
              ReturnProc  cb -> SLV S.empty
      t l = \ctx (SLV t) ->
        let (SLV killSet) = kill ctx l
            (SLV genSet)  = gen  ctx l
        in SLV $ (t S.\\ killSet) `S.union` genSet

    in M.fromList [(l, t l) | (l, _) <- b]

-- Constant Propagation

-- Evalueer een expressie. Expressies bestaan als condition of als rechterhand
-- van een assignment. Die berekeningen zijn puur dus de variabele omgeving
-- hoeft niet te worden vernieuwd in deze evaluator.
--
-- Als variabelen niet gevonden worden in de omgeving is het resultaat ⊤
eval :: Map String D -> Expr -> D
eval env arg =
  case arg of
    (I expr) ->
      let
        iOp f l r =
          let (D left ) = eval env (I l)
              (D right) = eval env (I r)
          in case (left, right) of
            (Bottom, _) -> D Bottom
            (_, Bottom) -> D Bottom
            (Top, _)    -> D Top
            (_, Top)    -> D Top
            (Val (PrimitiveInt leftRes), Val (PrimitiveInt rightRes)) ->
              D . Val . PrimitiveInt $ (f leftRes rightRes)
      in case expr of
        IConst   x -> D . Val . PrimitiveInt $ x
        Var    var -> M.findWithDefault (D Top) var env
        Plus   l r -> iOp (+)   l r
        Minus  l r -> iOp (-)   l r
        Times  l r -> iOp (*)   l r
        Divide l r -> iOp (div) l r
        -- Voor nu, omdat er nog geen dereferentiesemantiek bestaat. Ook: Waarom
        -- zijn er geen pointers naar booleans?
        Deref  ptr -> D Top
    (B expr) ->
      let
        -- Integer operator (met bijbehorende pattern matches)
        iOp f l r =
          let (D (Val (PrimitiveInt left ))) = eval env (I l)
              (D (Val (PrimitiveInt right))) = eval env (I r)
          in D . Val . PrimitiveBool $ (f left right)
        -- Boolse operator (met bijbehorende pattern matches)
        bOp f l r =
          let (D (Val (PrimitiveBool left ))) = eval env (B l)
              (D (Val (PrimitiveBool right))) = eval env (B r)
          in D . Val . PrimitiveBool $ (f left right)
      in case expr of
        BConst         x -> D . Val . PrimitiveBool $ x
        BVar         var -> M.findWithDefault (D Top) var env
        LessThan     l r -> iOp (<)  l r
        GreaterThan  l r -> iOp (>)  l r
        LessEqual    l r -> iOp (<=) l r
        GreaterEqual l r -> iOp (>=) l r
        IEqual       l r -> iOp (==) l r

        BEqual       l r -> bOp (==) l r
        And          l r -> bOp (&&) l r
        Or           l r -> bOp (||) l r
        Not          val ->
          let (D (Val (PrimitiveBool res))) = eval env (B val)
          in D . Val . PrimitiveBool $ (not res)

-- Zo kunnen we primitieve datatypes uitbreiden met ⊥ en ⊤
data Domain a
  = Top
  | Val a
  | Bottom
  deriving Eq

instance Ord a => Ord (Domain a) where
  compare Bottom  Bottom  = EQ          -- ⊥ = ⊥
  compare Top     Top     = EQ          -- ⊤ = ⊤
  compare _       Bottom  = GT          -- ∀x ≠ ⊥: x > ⊥
  compare Top     _       = GT          -- ∀x ≠ ⊤: ⊤ > x
  compare _       Top     = LT          -- ∀x ≠ ⊤: x < ⊤ 
  compare Bottom  _       = LT          -- ∀x ≠ ⊥: ⊥ < x
  compare (Val a) (Val b) = compare a b -- ∀ (x, y) ∉ {⊤, ⊥}²

instance Show a => Show (Domain a) where
  show Top     = "⊤"
  show Bottom  = "⊥"
  show (Val a) = show a

data Primitive
  = PrimitiveBool Bool
  | PrimitiveInt Int
  deriving (Eq, Ord)

instance Show Primitive where
  show (PrimitiveBool x) = show x
  show (PrimitiveInt  x) = show x

newtype D = D { unD :: Domain Primitive } deriving (Eq, Ord)

instance Show D where
  show (D x) = show x

newtype Analysis_CP = CP { unCP :: Map String D } deriving Eq

instance Show Analysis_CP where
  show (CP x) = show x

instance Lattice Analysis_CP where
  bottom       = const (CP M.empty)

  joinl (CP x) (CP y) =
    let
      mapToSet = S.fromList . M.toList

      xset = mapToSet x
      yset = mapToSet y

      -- Unieke delen kunnen blind toegevoegd worden aan de unie
      xUnique =  xset S.\\ yset
      yUnique =  yset S.\\ xset

      -- Gemeenschappelijke delen kunnen alleen worden toegevoegd als er geen
      -- elementen met dezelfde variabelenaam een andere waarde hebben.
      xCommon = xset S.\\ xUnique
      yCommon = yset S.\\ yUnique

      f (lvar, lval) (rvar, rval)
        | lvar == rvar = Just $ if lval == rval
                                then (lvar, lval)
                                else (lvar, D Top)
        | otherwise    = Nothing

      union = S.fromList
        [fromJust (f x y) | x <- S.toList xCommon
                          , y <- S.toList yCommon
                          , isJust (f x y)]


    in CP (M.fromList . S.toList $ S.unions [xUnique, yUnique, union])

instance SetRepr Analysis_CP where
  type Elem Analysis_CP = (String, D)
  toSet (CP x) = S.fromList . M.toList $ x

instance Transferable Analysis_CP where
  transfers b =
    let
      t l ctx = 
        case lookup l b of
          Nothing -> analysisLookupError l
          Just x  ->
            case x of
              Boolean expr   -> id
              Statement stat ->
                case stat of
                  Skip'     _     -> id
                  IAssign'  _ x a -> \(CP env) -> CP $ M.insert x (eval env (I a)) env
                  BAssign'  _ x a -> \(CP env) -> CP $ M.insert x (eval env (B a)) env
                  Continue' _     -> id
                  Break'    _     -> id
              IsProc      (ProcBlock n1 ps o1) -> case ctx of
                                  [] -> id
                                        -- Lookup the label of the last call in the context
                                  _  -> case lookup (head ctx) b of
                                            -- Replace the parameters in a proc by the expressions passed by the call
                                          Just (CallProc (CallBlock n2 es o2)) -> \(CP env) ->
                                              CP $ foldr (\(k,a) -> M.insert k (eval env (a))) env (zip ps es)
                                          _ -> analysisLookupError (head ctx)
              EndProc     (ProcBlock n1 ps o1) -> case ctx of
                                  [] -> id
                                        -- Lookup the label of the last call in the context
                                  _  -> case lookup (head ctx) b of
                                            -- Replace the result variable of the call with the result assigned to said variable
                                          Just (CallProc (CallBlock n2 es o2)) -> \(CP env) ->
                                              case M.lookup o1 env of
                                                Nothing  -> CP env -- The output variable is not assigned to
                                                (Just e) -> CP $ M.insert o2 e env
                                          _ -> analysisLookupError (head ctx)
              CallProc    cb -> id
              ReturnProc  cb -> id
    in M.fromList [ (l, t l) | (l, _) <- b]

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

-- Error functies
analysisLookupError :: Label -> a
analysisLookupError l =
  error $ "label " ++ show l ++ " does not occur in the analysis"
