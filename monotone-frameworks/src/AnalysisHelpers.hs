module AnalysisHelpers where

import AttributeGrammar

-- Helper functies, voor nu zijn dat de helpers voor de kill-gen sets

-- Alle variabelen in een expressie
variables :: Expr -> [String]
variables (I e) =
  case e of
    IConst _ -> []
    Var name -> [name]
    Plus   l r -> variables (I l) ++ variables (I r)
    Minus  l r -> variables (I l) ++ variables (I r)
    Times  l r -> variables (I l) ++ variables (I r)
    Divide l r -> variables (I l) ++ variables (I r)
    Deref  ptr -> variables (I ptr)
variables (B e) =
  case e of
    BConst _  -> []
    BVar name -> [name]
    LessThan     l r -> variables (I l) ++ variables (I r)
    GreaterThan  l r -> variables (I l) ++ variables (I r)
    LessEqual    l r -> variables (I l) ++ variables (I r)
    GreaterEqual l r -> variables (I l) ++ variables (I r)
    IEqual       l r -> variables (I l) ++ variables (I r)
    BEqual       l r -> variables (B l) ++ variables (B r)
    And          l r -> variables (B l) ++ variables (B r)
    Or           l r -> variables (B l) ++ variables (B r)
    Not val -> variables (B val)

-- Vrije variabelen

-- Controleert of een variabele @v@ vrij is in @expr@. Dat is het geval desda v
-- niet genoemd wordt in een van de variabelen in @expr@. Als hij wel genoemd
-- wordt dan is hij niet vrij.
freeIn, notFreeIn :: String -> Expr -> Bool
freeIn    v expr = not (notFreeIn v expr)
notFreeIn v expr = v `elem` (variables expr)


-- Expressions
expressions :: Expr -> [Expr]
expressions (I e) =
  case e of
    IConst _ -> []
    Var _    -> []
    Plus   l r -> concat [[I (Plus   l r)], expressions (I l), expressions (I r)]
    Minus  l r -> concat [[I (Minus  l r)], expressions (I l), expressions (I r)]
    Times  l r -> concat [[I (Times  l r)], expressions (I l), expressions (I r)]
    Divide l r -> concat [[I (Divide l r)], expressions (I l), expressions (I r)]
    Deref  ptr -> expressions (I ptr)
expressions (B e) =
  case e of
    BConst _  -> []
    BVar _    -> []
    LessThan     l r -> concat [[B (LessThan     l r)], expressions (I l), expressions (I r)]
    GreaterThan  l r -> concat [[B (GreaterThan  l r)], expressions (I l), expressions (I r)]
    LessEqual    l r -> concat [[B (LessEqual    l r)], expressions (I l), expressions (I r)]
    GreaterEqual l r -> concat [[B (GreaterEqual l r)], expressions (I l), expressions (I r)]
    IEqual       l r -> concat [[B (IEqual       l r)], expressions (I l), expressions (I r)]
    BEqual       l r -> concat [[B (BEqual       l r)], expressions (B l), expressions (B r)]
    And          l r -> concat [[B (And          l r)], expressions (B l), expressions (B r)]
    Or           l r -> concat [[B (Or           l r)], expressions (B l), expressions (B r)]
    Not val -> expressions (B val)

statExpressions :: Stat' -> [Expr]
statExpressions e =
  case e of
    Skip' _ -> []
    IfThenElse'  _ cond stat1 stat2 ->
      expressions (B cond) ++ concatMap statExpressions [stat1, stat2]
    While' _ cond stat ->
      expressions (B cond) ++ (statExpressions stat)
    Call' _ _ _ _ _    -> [] -- TODO: Inter
    IAssign' _ _ val   -> expressions (I val)
    BAssign' _ _ val   -> expressions (B val)
    Seq' l r           -> concatMap statExpressions [l, r]
    Malloc' _ _ size   -> expressions (I size)
    Free' _ ptr        -> expressions (I ptr)
    RefAssign' _ ptr val -> concatMap expressions [I ptr, I val]
    Continue' _        -> []
    Break'    _        -> []
