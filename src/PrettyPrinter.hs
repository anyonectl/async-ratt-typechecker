module PrettyPrinter where

import Syntax

instance Show Chan where 
  show (Push ty)    = "push (" ++ show ty ++ ")"
  show (Buf ty)     = "buf (" ++ show ty ++ ")"
  show (PushBuf ty) = "pushbuf (" ++ show ty ++ ")"

instance Show ClockExpr where
  show (Clock v)            = "cl(" ++ show v ++ ")"
  show (ClockUnion cl cl')  = "(" ++ show cl ++ " |_| " ++ show cl' ++ ")"

instance Show Type where
  show Unit           = "()"
  show Nat            = "Nat"
  show (Prod ty ty')  = "(" ++ show ty ++ ", " ++ show ty' ++ ")"
  show (Sum ty ty')   = "(" ++ show ty ++ " + " ++ show ty' ++ ")"
  show (Fun ty ty')   = "(" ++ show ty ++ " -> " ++ show ty' ++ ")"
  show (EL ty)        = "EL (" ++ show ty ++ ")"
  show (AL ty)        = "AL (" ++ show ty ++ ")"
  show (Box ty)       = "□(" ++ show ty ++ ")"
  show (Sig ty)       = "Sig " ++ show ty ++ ""

instance Show Term where
  show (Variable x)         = "\"" ++ x ++ "\""
  show UnitVal              = "()"
  show Zero                 = "0"
  show (Suc t)              = "(" ++ show t ++ " + 1)"
  show (Pair t t')          = "(" ++ show t ++ ", " ++ show t' ++ ")"
  show (Inj i t)            = "(" ++ show i ++ " (" ++ show t ++ "))"
  show (Proj i t)           = "(" ++ show i ++ " (" ++ show t ++ "))"
  show (Let x s t)          = 
    "(let " ++ show x ++ " = " ++ show s ++ 
    " in " ++ show t ++ ")"
  show (Lam x t)            = "(\\" ++ show x ++ " . " ++ show t ++ ")"
  show (App t t')           = "(" ++ show t ++ " (" ++ show t' ++ "))"
  show (Case t x1 t1 x2 t2) = 
    "(case " ++ show t ++ " of " ++ 
    "(" ++ show InLeft ++ " " ++ show x1 ++ " -> " ++ show t1 ++ ")" ++ " , " ++
    "(" ++ show InRight ++ " " ++ show x2 ++ " -> " ++ show t2 ++ ")"
  show (Delay cl t)         = "delay (" ++ show cl ++ " " ++ show t ++ ")"
  show (Adv v)              = "adv " ++ show v
  show (AdvAL v)            = "advAL " ++ show v
  show (Select v1 v2)       = "select (" ++ show v1 ++ " " ++ show v2 ++ ")"
  show (Always t)           = "box " ++ show t
  show (Unbox t)            = "unbox " ++ show t
  show (Fix x t)            = "fix " ++ show x ++ " . " ++ show t
  show Never                = "never"
  show (Wait k)             = "wait " ++ show k
  show (Read k)             = "read " ++ show k
  show (Head s)             = "head " ++ show s
  show (Tail s)             = "tail " ++ show s
  show (Cons hd tl)         = "(" ++ show hd ++ " : " ++ show tl ++ ")"
  show (Anno t ty)          = "(" ++ show t ++ " :: " ++ show ty ++ ")"

instance Show Injection where
  show InLeft = "left"
  show InRight = "right"

instance Show Projection where
  show Fst = "fst"
  show Snd = "snd"