module Syntax where

-- INPUT CHANNELS
data Chan = P Type      -- push channel
          | B Type      -- buffered channel
          | BP Type     -- buffered push channel

chanType :: Chan -> Type
chanType (P A)  = A
chanType (B A)  = A
chanType (BP A) = A

chanClass :: Chan -> String
chanClass P  = "Push"
chanClass B  = "Buffered"
chanClass BP = "BufferedPush"

isPush :: Chan -> Bool
isPush (B _) = False
_            = True

-- CLOCKS
data ClockExpr = ClockOf Term
               | ClockUnion ClockExpr ClockExpr

-- VARIABLE
type Var = String   -- type to represent variables

-- TYPING CONTEXT
type SimpleCtx = [(Var, Type)]

type Ctx = (SimpleCtx, [(ClockExpr, SimpleCtx)])

emptyCtx :: Ctx
emptyCtx = ([], []) -- the empty typing context       

-- Add variable binding  x : A to a typing context
addVarBind :: Var -> Type -> Ctx -> Ctx
addVarBind v t ctx = ((v, t) :: ctx.fst, ctx.snd)

-- Add a tick on some clock to a typing context
addTick :: ClockExpr -> Ctx -> Ctx
addTick cl ctx = ([], (cl, ctx.fst) :: ctx.snd)

-- TYPES
data Type = One                         -- simplest possible type, with exactly one value: () (unit)
          | Nat                         -- type of non-negative integers 0, 1, 2, ...
          | Prod Type Type              -- A x B is a pair (2-tuple)
          | Sum Type Type               -- A + B - the type of values that are EITHER from A or from B
          | Fun Type Type               -- type of functions from A to B (lambda)
          | EL Type                     -- delayed computation on a local clock (sometime later); a pair of 1) a clock, and 2) a suspended computation that produces a value of type A the next time the clock ticks
          | AL Type                     -- delay on the global clock (anytime later); a value of type this AL type is a computation that will produce a value of type A after any clock ticks
          | Always Type                 -- type of a timeless computation (the stable modal operator, pronounced 'box')

-- Todo: Is a type a stable type? Is a type a value type? Helper functions?
isStable :: Type -> Bool
isStable One = True
isStable Nat = True
isStable (AL _) = True
isStable (Always _) = True
isStable (Prod t t') = if isStable t && isStable t' then True else False
isStable (Sum t t') = if isStable t && isStable t' then True else False
isStable _ = False


isValue :: Type -> Bool

-- TERMS
data Term = Var Var                     -- variable
          | Zero                        -- the constant 0 always has type Nat
          | Unit                        -- unit                     
          | Suc Term                    -- +1 (suc t alays has type Nat)
          | Pair Term Term              -- pair (has type Tuple)
          | In Int Term                 -- sum injection (corresponds to Left and Right in Haskell)
          | Pi Int Term                 -- projection (extracts one component from a pair, like fst / snd in Haskell)
          | Let Var Term Term           -- let binding (how: need a type to represent variables)
          | Lambda Var Term             -- function (lambda x.t)
          | App Term Term               -- function application (apply t to t')
          | Case Term Var Term Var Term -- case analysis (pattern matching on a sum type) (not completely sure why it has to carry `Term Var Term Var Term` ...)
          | Delay ClockExpr Term        -- correct (has type EL A)
          | Adv Term                    -- Term must be a value (correct)
          | AdvAL Term                  -- advance a anytime-later computation (correct)
          | Select Term Term            -- synchronising two delayed computations / find out which clock fired
          | Box Term                    -- create a stable computation
          | Unbox Term                  -- use a stable computation  (correct)
          | Never                       -- correct
          | Wait Chan                   -- InputChannel must be P or BP
          | Read Chan                   -- InputChannel must be B or BP

-- Todo: Is a term a value (cant be reduced more)? (Is it even important?)
isValue :: Term -> Bool