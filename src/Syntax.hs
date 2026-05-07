module Syntax where

-- INPUT CHANNELS 
data Chan
    = Push Type
    | Buf Type
    | PushBuf Type
    deriving (Eq)

isPush :: Chan -> Bool
isPush (Push _)    = True
isPush (PushBuf _) = True
isPush (Buf _)     = False

isBuf :: Chan -> Bool
isBuf (Buf _)     = True
isBuf (PushBuf _) = True
isBuf (Push _)    = False

chanType :: Chan -> Type
chanType (Buf ty)      = ty
chanType (PushBuf ty)  = ty
chanType (Push ty)     = ty

-- Input channel context (Delta) with hardcoded example channels
inputChans :: [Chan]
inputChans = [Push Nat, Buf Nat, PushBuf Nat]

-- CLOCKS
-- Right now, clocks and channels are not associated in any way, even though a clock is a set of (push) channels, right? should we change this?
data ClockExpr 
    = Clock Term
    | ClockUnion ClockExpr ClockExpr
    deriving (Eq)


-- VARIABLES
type Var = String


-- TYPES
data Type 
    = Unit                        -- simplest possible type, with exactly one value: () (unit)
    | Empty
    | Nat                         -- type of non-negative integers 0, 1, 2, ... 
    | Prod Type Type              -- A x B is a pair (2-tuple)
    | Sum Type Type               -- A + B - the type of values that are EITHER from A or from B
    | Fun Type Type               -- type of functions from A to B (lambda)
    | EL Type                     -- delayed computation on a local clock (sometime later); a pair of 1) a clock, and 2) a suspended computation that produces a value of type A the next time the clock ticks
    | AL Type                     -- delay on the global clock (anytime later); a value of type this AL type is a computation that will produce a value of type A after any clock ticks
    | Box Type                    -- type of a timeless computation (the stable modal operator, pronounced 'box')
    | Sig Type                    -- Sig A is a signal of type A
    deriving (Eq)

isStableType :: Type -> Bool
isStableType Unit        = True
isStableType Nat         = True
isStableType (Prod t t') = isStableType t && isStableType t'
isStableType (Sum t t')  = isStableType t && isStableType t'
isStableType (AL _)      = True
isStableType (Box _)     = True
isStableType _           = False

isValueType :: Type -> Bool
isValueType Unit        = True
isValueType Nat         = True
isValueType (Prod t t') = isValueType t && isValueType t'
isValueType (Sum t t')  = isValueType t && isValueType t'
isValueType _           = False


-- TERMS
data Term 
    = Variable Var                -- variable
    | UnitVal                     -- unit
    | Zero                        -- the constant 0 always has type Nat
    | Suc Term                    -- +1 (suc t alays has type Nat)
    | Pair Term Term              -- pair (has type Tuple)
    | Inj Injection Term          -- sum injection (corresponds to Left and Right in Haskell)
    | Proj Projection Term        -- projection (extracts one component from a pair, like fst / snd in Haskell)
    | Let Var Term Term           -- let binding (how: need a type to represent variables)
    | Lam Var Term                -- function (lambda x.t)
    | App Term Term               -- function application (apply t to t')
    | Case Term Var Term Var Term -- case analysis (pattern matching on a sum type)    
    | Delay ClockExpr Term        -- has type EL A
    | Adv Term                    -- Term must be a value
    | AdvAL Term                  -- advance a anytime-later computation
    | Select Term Term            -- synchronising two delayed computations / find out which clock fired. Can only be applied to values
    | Always Term                 -- create a stable computation
    | Unbox Term                  -- use a stable computation
    | Fix Var Term                -- recursion (guarded?)
    | Never                       -- has type EL A
    | Wait Chan                   -- InputChannel must be P or BP
    | Read Chan                   -- InputChannel must be B or BP
    | Head Term                   
    | Tail Term
    | Cons Term Term
    | Anno Term Type 
    deriving (Eq)

data Injection  = InLeft | InRight deriving (Eq)
data Projection = Fst    | Snd     deriving (Eq)

-- Check if a term is a value (can't be reduced further) - relevant for `advance` and `select`
isValue :: Term -> Bool
isValue (Variable _) = True
isValue UnitVal      = True
isValue Zero         = True
isValue (Suc v)      = isValue v
isValue (Pair v w)   = isValue v && isValue w
isValue (Inj _ v)    = isValue v
isValue (Lam _ _)    = True
isValue (Always _)   = True
isValue (Wait _)     = True
isValue _            = False


-- SHORTHAND
(-->) :: Type -> Type -> Type
(-->) = Fun
infixr 1 -->

var :: Var -> Term
var = Variable

(@@) :: Term -> Term -> Term
(@@) = App
infixl 9 @@

lam :: Var -> Term -> Term
lam = Lam

let_ :: Var -> Term -> Term -> Term
let_ x t1 t2 = Let x t1 t2


-- PRETTY PRINTER
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
  show (Sig ty)       = "Sig " ++ show ty
  show Empty          = "`empty type`"

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