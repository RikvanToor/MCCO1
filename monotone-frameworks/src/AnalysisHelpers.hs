module AnalysisHelpers where

import AttributeGrammar

-- Helper functies, voor nu zijn dat de helpers voor de kill-gen sets

-- Free variables
notFreeIn :: String -> Expr -> Bool
notFreeIn v (I e) =
  case e of
    IConst _ -> False
    Var name -> v /= name
    Plus   l r -> all (notFreeIn v . I) [l, r]
    Minus  l r -> all (notFreeIn v . I) [l, r]
    Times  l r -> all (notFreeIn v . I) [l, r]
    Divide l r -> all (notFreeIn v . I) [l, r]
    Deref  ptr -> notFreeIn v (I ptr)
notFreeIn v (B e) =
  case e of
    BConst _  -> False
    BVar name -> v /= name
    LessThan     l r -> all (notFreeIn v . I) [l, r]
    GreaterThan  l r -> all (notFreeIn v . I) [l, r]
    LessEqual    l r -> all (notFreeIn v . I) [l, r]
    GreaterEqual l r -> all (notFreeIn v . I) [l, r]
    IEqual       l r -> all (notFreeIn v . I) [l, r]
    BEqual       l r -> all (notFreeIn v . B) [l, r]
    And          l r -> all (notFreeIn v . B) [l, r]
    Or           l r -> all (notFreeIn v . B) [l, r]
    Not val -> notFreeIn v (B val)

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
