module Syntax where

-- INPUT CHANNELS 
data Chan
    = Push Type
    | Buf Type
    | PushBuf Type
    deriving (Eq, Show)

isPush :: Chan -> Bool
isPush (Push _)    = True
isPush (PushBuf _) = True
isPush (Buf _)     = False

isBuf :: Chan -> Bool
isBuf (Buf _)     = True
isBuf (PushBuf _) = True
isBuf (Push _)    = False

-- -- Relevant for `wait` and `read`
-- chanClass :: Chan -> String -> Bool
-- chanClass chan "b" = isBuf chan
-- chanClass chan "p" = isPush chan


-- CLOCKS
data ClockExpr 
    = Clock Term
    | ClockUnion ClockExpr ClockExpr
    deriving (Eq, Show)


-- VARIABLES
-- !!!
-- TODO consider if we should implement this as type to not get crazy from writing tests.. 
-- !!!
newtype Var = Var String deriving (Eq, Show)

-- TYPING CONTEXTS
type SCtx = [(Var, Type)] 

data Ctx
    = EmptyCtx
    | Ctx (SCtx, [(SCtx, ClockExpr)])
    deriving (Eq, Show)


-- TYPES
data Type 
    = Unit                        -- simplest possible type, with exactly one value: () (unit)
    | Nat                         -- type of non-negative integers 0, 1, 2, ... 
    | Prod Type Type              -- A x B is a pair (2-tuple)
    | Sum Type Type               -- A + B - the type of values that are EITHER from A or from B
    | Fun Type Type               -- type of functions from A to B (lambda)
    | EL Type                     -- delayed computation on a local clock (sometime later); a pair of 1) a clock, and 2) a suspended computation that produces a value of type A the next time the clock ticks
    | AL Type                     -- delay on the global clock (anytime later); a value of type this AL type is a computation that will produce a value of type A after any clock ticks
    | Box Type                    -- type of a timeless computation (the stable modal operator, pronounced 'box')
    | Sig Type                    -- Sig A is a signal of type A
    deriving (Eq, Show)

isStable :: Type -> Bool
isStable Unit        = True
isStable Nat         = True
isStable (Prod t t') = isStable t && isStable t'
isStable (Sum t t')  = isStable t && isStable t'
isStable (AL _)      = True
isStable (Box _)     = True
isStable _           = False

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
    | Never
    | Wait Chan                   -- InputChannel must be P or BP
    | Read Chan                   -- InputChannel must be B or BP
    | Head Term                   
    | Tail Term
    | Cons Term Term
    deriving (Eq, Show)

data Injection = InLeft | InRight deriving (Eq, Show)
data Projection = Fst | Snd deriving (Eq, Show)

true :: Term
true = Inj InLeft UnitVal

false :: Term
false = Inj InRight UnitVal

bool :: Type
bool = Sum Unit Unit

isTrue :: Term -> Bool
isTrue (Inj InLeft UnitVal) = True
isTrue _                    = False

isFalse :: Term -> Bool
isFalse (Inj InRight UnitVal) = True
isFalse _                     = False

isBoolType :: Type -> Bool
isBoolType (Sum Unit Unit) = True  -- Checks if it's the sum of two Units
isBoolType _               = False -- Otherwise, it's not a boolean type

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

