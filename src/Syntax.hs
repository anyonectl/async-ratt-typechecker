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
    | Empty                       -- Empty type
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
isStableType Empty       = True
isStableType Nat         = True
isStableType (Prod t t') = isStableType t && isStableType t'
isStableType (Sum t t')  = isStableType t && isStableType t'
isStableType (AL _)      = True
isStableType (Box _)     = True
isStableType _           = False -- Sig _, EL _, Fun _ _

isValueType :: Type -> Bool
isValueType Unit        = True
isValueType Nat         = True
isValueType (Prod t t') = isValueType t && isValueType t'
isValueType (Sum t t')  = isValueType t && isValueType t'
isValueType _           = False -- Empty, Fun _ _, EL _, AL _, Box _, Sig _


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
instance Show Type where
  show = prettyType

instance Show Chan where
  show = prettyChan

instance Show ClockExpr where
  show = prettyClock

instance Show Term where
  show = prettyTerm 0

instance Show Injection where
  show InLeft  = "left"
  show InRight = "right"

instance Show Projection where
  show Fst = "fst"
  show Snd = "snd"

indent :: Int -> String
indent n = replicate (2 * n) ' '

prettyType :: Type -> String
prettyType Unit          = "()"
prettyType Empty         = "`empty type`"
prettyType Nat           = "Nat"
prettyType (Prod ty ty') = "(" ++ prettyType ty ++ " × " ++ prettyType ty' ++ ")"
prettyType (Sum ty ty')  = "(" ++ prettyType ty ++ " + "  ++ prettyType ty' ++ ")"
prettyType (Fun ty ty')  = "(" ++ prettyType ty ++ " -> " ++ prettyType ty' ++ ")"
prettyType (EL ty)       = "EL (" ++ prettyType ty ++ ")"
prettyType (AL ty)       = "AL (" ++ prettyType ty ++ ")"
prettyType (Box ty)      = "□(" ++ prettyType ty ++ ")"
prettyType (Sig ty)      = "Sig " ++ prettyType ty

prettyChan :: Chan -> String
prettyChan (Push ty)    = "push ("    ++ prettyType ty ++ ")"
prettyChan (Buf ty)     = "buf ("     ++ prettyType ty ++ ")"
prettyChan (PushBuf ty) = "pushbuf (" ++ prettyType ty ++ ")"

prettyClock :: ClockExpr -> String
prettyClock (Clock t)           = "cl(" ++ prettyTerm 0 t ++ ")"
prettyClock (ClockUnion cl cl') = prettyClock cl ++ " |_| " ++ prettyClock cl'

prettyTerm :: Int -> Term -> String
prettyTerm n term =
  case term of
    Variable x          -> x
    UnitVal             -> "()"
    Zero                -> "0"
    Never               -> "never"
    Wait k              -> "wait " ++ prettyChan k
    Read k              -> "read " ++ prettyChan k
    Suc t               -> "(" ++ prettyTerm n t ++ ") + 1"
    Adv v               -> "adv " ++ prettyTerm n v
    AdvAL v             -> "advAL " ++ prettyTerm n v
    Unbox t             -> "unbox (" ++ prettyTerm n t ++ ")"
    Head s              -> "head (" ++ prettyTerm n s ++ ")"
    Tail s              -> "tail (" ++ prettyTerm n s ++ ")"
    Always t            -> "box (" ++ prettyTerm n t ++ ")"
    Proj p t            -> show p ++ " (" ++ prettyTerm n t ++ ")"
    Inj  i t            -> show i ++ " " ++ prettyTerm n t
    Anno t ty           -> prettyTerm n t ++ " : " ++ prettyType ty
    Pair t t'           ->
      "(" ++ prettyTerm n t ++ ", " ++ prettyTerm n t' ++ ")"
    Select v1 v2        -> 
      "select " ++ prettyTerm n v1 ++ " " ++ prettyTerm n v2

    Lam x t             ->
      "\\" ++ x ++ ".\n" ++ 
      indent (n+1) ++ prettyTerm (n+1) t

    App t t'            ->
      "(" ++ prettyTerm n t ++ ") @\n" ++ 
      indent (n+1) ++ "(" ++ prettyTerm (n+1) t' ++ ")"

    Let x s t           ->
      "let " ++ x ++ " = " ++ prettyTerm (n+1) s ++ " in\n" ++
      indent (n+1) ++ prettyTerm (n+1) t

    Case t x1 t1 x2 t2  ->
      "case (" ++ prettyTerm n t ++ ") of\n" ++
      indent (n+1) ++ "left " ++ x1 ++ " ->\n" ++
      indent (n+2) ++ prettyTerm (n+2) t1 ++ "\n" ++
      indent (n+1) ++ "right " ++ x2 ++ " ->\n" ++
      indent (n+2) ++ prettyTerm (n+2) t2

    Delay cl t ->
      "delay\n" ++ 
      indent (n+1) ++ prettyClock cl ++ "\n" ++
      indent (n+1) ++ prettyTerm (n+1) t

    Fix x t ->
      "fix " ++ x ++ " .\n" ++
      indent (n+1) ++ prettyTerm (n+1) t

    Cons hd tl ->
      prettyTerm n hd ++ " ::\n" ++
      indent (n+1) ++ prettyTerm (n+1) tl