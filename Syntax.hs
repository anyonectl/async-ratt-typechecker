module Syntax where

-- LOCATIONS AND INPUT CHANNELS.. AND VARIABLE
-- These are just temporary aliases.. 
-- type Loc = String
-- type Chan = String -- b, bp, p
type Variable   = String 


-- CLOCK EXPRESSIONS
data ClockExpr 
    = Clock Term
    | SqUnion ClockExpr ClockExpr


-- TYPING CONTEXT

{-  Q: From what Patrick wrote on the board, but I'm not sure about where 
    the context with only tick fits in?

    Q: Also, what example from the paper should be look at? 
-}

type SCtxt = [(Variable, Type)] -- Simple Context: mapping from variable to type (which has a value)

data Ctxt
    = EmptyCtxt
    | Ctxt [(SCtxt, ClockExpr)]


-- TYPES
data Type 
    = Unit                     
    | Nat
    | Prod Type Type
    | Sum Type Type
    | Fun Type Type 
    | EL Type           -- ⊛∃ A
    | AL Type           -- ⊛∀ A
    | Box Type          -- □ A (renamed Boxed because of Box function)
    -- | Fix Var Type     -- Fix α. A
    -- | Wait Chan
    -- | Into Type

isStable :: Type -> Bool
isStable Unit        = True
isStable Nat         = True
isStable (Prod s s') = isStable s && isStable s'
isStable (Sum s s')  = isStable s && isStable s'
isStable (AL _)      = True
isStable (Box _)     = True
isStable _           = False -- Fun, EL 

isValueType :: Type -> Bool
isValueType Unit        = True
isValueType Nat         = True
isValueType (Prod s s') = isValueType s && isValueType s'
isValueType (Sum s s')  = isValueType s && isValueType s'
isValueType _           = False -- Fun, EL, AL, Box


-- TERMS
data Injection  = Inl | Inr
data Projection = Fst | Snd

{-  Q: I (still) don't get the Value completely..
    Why can we not have it nested? Should it be a helper 
    function instead or should we simply expand the "Term" to 
    include them? Look below for helper function. 
-}

data Term 
    = Var    Variable 
    | UniVal  -- ()
    | Zero    -- 0
    | Suc    Term
    | Lambda Variable Term
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
    | BoxVal Term -- value
    | Unbox  Term              -- OK
    | Never                   -- OK
---    | Read Chan -- do we need lexier here to parse chan (which at this point is a) string? 

isValue :: Term -> Bool
isValue (Var v)    = True -- x
isValue UniVal     = True -- ()
isValue Zero       = True -- 0
isValue (Suc v)    = isValue v
isValue (Pair s t) = isValue s && isValue t
isValue (Inj _ v)  = isValue v
isValue (BoxVal t) = True -- t is term, not value so don't need to recurse