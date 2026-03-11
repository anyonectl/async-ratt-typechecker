module Syntax where

-- LOCATIONS AND INPUT CHANNELS
-- These are just temporary aliases.. 
-- type Loc = String
-- type Chan = String -- b, bp, p

-- CLOCK EXPRESSIONS
data ClockExpr 
    = Clock Term
    | SqUnion ClockExpr ClockExpr

-- TYPING CONTEXT
type SCtxt = [(Variable, Type)] -- Simple Context: mapping from variable to type (which has a value)

data Ctxt
    = EmptyCtxt
    | Ctxt [(SCtxt, ClockExpr)]

-- Peano Natural Numbers
data PeanoNat
    = Zero          -- Fig. 3 (21)
    | Succ PeanoNat -- Fig. 3 (22)

-- TYPES
data Type 
    = Unit                     
    | Nat PeanoNat        -- should not take an Int because it's a TYPE
    | Prod Type Type
    | Sum Type Type
    | Fun Type Type 
    | EL Type           -- ⊛∃ A
    | AL Type           -- ⊛∀ A
    | Boxed Type          -- □ A (renamed Boxed because of Box function)
    -- | Fix Var Type     -- Fix α. A
    -- | Wait Chan
    -- | Into Type

isStable :: Type -> Bool
isStable Unit        = True
isStable (Nat n)     = True -- Should check NatVal? 
isStable (Prod s s') = isStable s && isStable s'
isStable (Sum s s')  = isStable s && isStable s'
isStable (AL _)      = True
isStable (Boxed _)   = True
isStable _           = False -- Fun, EL 

isValueType :: Type -> Bool
isValueType Unit        = True
isValueType (Nat n)     = True -- Should check NatVal? 
isValueType (Prod s s') = isValueType s && isValueType s'
isValueType (Sum s s')  = isValueType s && isValueType s'
isValueType _           = False -- Fun, EL, AL, Box

-- TERMS
data Injection  = Inl | Inr
data Projection = Fst | Snd
type Variable   = String 

-- Q: I don't get this one.. Why can we not have it nested? Should it be a helper function instead or should we simply expand the "Term" to include them?
data Term 
    = Var    Variable 
    | Suc    PeanoNat -- Not sure about this.. 
    | Pair   Term Term        -- (s,t)
    | Inj    Injection Term    -- left or right
    | Proj   Projection Term  -- fst or snd
    | Let    Variable Term Term 
    | App    Term Term            
    | Case   Term Variable Term Term -- Q: we need a variable x here?
    | Delay  ClockExpr Term    -- OK
    | Adv    Term                -- OK
    | AdvAL  Term              -- OK
    | Select Term Term 
    | Box    Term -- value
    | Unbox  Term              -- OK
    | Never                   -- OK
---    | Read Chan -- do we need lexier here to parse chan (which at this point is a) string? 

-- For now.. helper function to check if the term is value or not
isValue :: Term -> Bool
isValue (Var v)    = True -- x
isValue (Pair s t) = isValue s && isValue t
-- isValue (Suc v) = isValue v -- isPeanoNat v? Helper function?
isValue (Inj i v)  = isInjection i && isValue v
isValue (Box t)    = True -- t is term, not value so don't need to recurse

isInjection :: Injection -> Bool
isInjection Inl = True
isInjection Inr = True