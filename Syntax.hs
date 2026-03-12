module Syntax where

-- INPUT CHANNELS
data ChanClass = Push
               | Buffered
               | BufferedPush

data Chan = Chan { chanClass :: ChanClass, chanType :: Type }

isPush :: Chan -> Bool
isPush (Chan Buffered _) = False
isPush _                 = True

isBuff :: Chan -> Bool
isPush (Chan Push _) = False
isPush _             = True

-- CLOCKS
data ClockExpr = ClockOf Term
               | ClockUnion ClockExpr ClockExpr

-- VARIABLE
newtype Var = Var String   -- type to represent variables

-- TYPING CONTEXT
type SimpleCtx = [(Var, Type)]

type Ctx = (SimpleCtx, [(ClockExpr, SimpleCtx)])

emptyCtx :: Ctx
emptyCtx = ([], []) -- the empty typing context       

-- Add variable binding  x : A to a typing context
addVarBind :: Var -> Type -> Ctx -> Ctx
addVarBind v T (simple, ticked) = ((v, T) : simple, ticked)

-- Add a tick on some clock to a typing context
addTick :: ClockExpr -> Ctx -> Ctx
addTick cl (simple, ticked) = ([], (cl, simple) : ticked)

-- TYPES
data Type = One                         -- simplest possible type, with exactly one value: () (unit)
          | Nat                         -- type of non-negative integers 0, 1, 2, ...
          | Prod Type Type              -- A x B is a pair (2-tuple)
          | Sum Type Type               -- A + B - the type of values that are EITHER from A or from B
          | Fun Type Type               -- type of functions from A to B (lambda)
          | EL Type                     -- delayed computation on a local clock (sometime later); a pair of 1) a clock, and 2) a suspended computation that produces a value of type A the next time the clock ticks
          | AL Type                     -- delay on the global clock (anytime later); a value of type this AL type is a computation that will produce a value of type A after any clock ticks
          | Always Type                 -- type of a timeless computation (the stable modal operator, pronounced 'box')

isStable :: Type -> Bool
isStable One         = True
isStable Nat         = True
isStable (AL _)      = True
isStable (Always _)  = True
isStable (Prod T T') = isStable T && isStable T'
isStable (Sum T T')  = isStable T && isStable T'
isStable _           = False

isValueType :: Type -> Bool
isValue One         = True
isValue Nat         = True
isValue (Prod T T') = isValue T && isValue T'
isValue (Sum T T')  = isValue T && isValue T'
isValue _           = False

-- TERMS
data Term = Variable Var                -- variable
          | Unit                        -- unit
          | Zero                        -- the constant 0 always has type Nat
          | Suc Term                    -- +1 (suc t alays has type Nat)
          | Pair Term Term              -- pair (has type Tuple)
          | Inj Injection Term          -- sum injection (corresponds to Left and Right in Haskell)
          | Proj Projection Term        -- projection (extracts one component from a pair, like fst / snd in Haskell)
          | Let Var Term Term           -- let binding (how: need a type to represent variables)
          | Lambda Var Term             -- function (lambda x.t)
          | App Term Term               -- function application (apply t to t')
          | Case Term Var Term Var Term -- case analysis (pattern matching on a sum type)
          | Delay ClockExpr Term        -- has type EL A
          | Adv Term                    -- Term must be a value
          | AdvAL Term                  -- advance a anytime-later computation
          | Select Term Term            -- synchronising two delayed computations / find out which clock fired
          | Box Term                    -- create a stable computation
          | Unbox Term                  -- use a stable computation
          | Never
          | Wait Chan                   -- InputChannel must be P or BP
          | Read Chan                   -- InputChannel must be B or BP

data Injection = InLeft | InRight
data Projection = Fst | Snd

-- Check if a term is a value (can't be reduced further)
isValue :: Term -> Bool
isValue (Variable _) = True
isValue Unit         = True
isValue Zero         = True
isValue (Suc v)      = isValue v
isValue (Lambda _ _) = True
isValue (Pair v w)   = isValue v && isValue w
isValue (In _ v)     = isValue v
isValue (Wait _)     = True
isValue (Box _)      = True
isValue _            = False