module Ast where

data Op 
  = Add | Sub | Mul | Div  
  deriving (Eq,Show)
  
type Pi    = Integer -- For numbering lambda's etc. that can then be tracked in the analysis
type Name  = String  -- For identifier names

data Expr
  = Integer Integer
  | Bool    Bool
  | Var     Name
  | Fun     Pi   Name Name Expr
  | Fn      Pi   Name Expr
  | App     Expr Expr
  | Let     Name Expr Expr
  | ITE     Expr Expr Expr
  | Oper    Op   Expr Expr
  deriving (Eq,Show)


bin :: Name -> Expr -> Expr -> Expr
bin op x y = Oper r x y where
  r = case op of
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div

        
