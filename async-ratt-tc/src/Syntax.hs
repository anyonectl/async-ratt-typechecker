module Syntax where

-- VARIABLES
type Var = String 


-- INPUT CHANNELS 
data Chan
    = Push Type
    | Buf Type
    | PushBuf Type

isPush :: Chan -> Bool
isPush (Push t)    = True
isPush (PushBuf t) = True
isPush (Buf t)     = False

isBuf :: Chan -> Bool
isBuf (Buf t)     = True
isBuf (PushBuf t) = True
isBuf (Push t)    = False

-- Relevant for `wait` and `read`
chanClass :: Chan -> String -> Bool
chanClass chan "b" = isBuf chan
chanClass chan "p" = isPush chan


-- CLOCK EXPRESSIONS
data ClockExpr 
    = Clock Term
    | SqUnion ClockExpr ClockExpr


-- TYPING CONTEXT
type SCtxt = [(Var, Type)]

data Ctxt
    = EmptyCtxt
    | Ctxt (SCtxt, [(SCtxt, ClockExpr)]) 


-- TYPES
data Type 
    = Unit                     
    | Nat
    | Prod Type Type
    | Sum Type Type
    | Fun Type Type 
    | EL Type           -- ⊛∃ A
    | AL Type           -- ⊛∀ A
    | Box Type          -- □ A
    | Sig Type          -- Sig A
    | Wait Chan

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

data Term 
    = TVar   Var                    -- x
    | UniVal                        -- ()
    | Zero                          -- 0
    | Suc    Term                   -- Successor 
    | Lambda Var Term               -- x . t 
    | Pair   Term Term              -- (s,t)
    | Inj    Injection Term         -- left or right
    | Proj   Projection Term        -- fst or snd
    | Let    Var Term Term          -- 
    | App    Term Term              -- s t  
    | Case   Term Var Term Var Term -- OK
    | Delay  ClockExpr Term         -- OK
    | Adv    Term                   -- tic
    | AdvAL  Term                   -- OK
    | Select Term Term 
    | BoxVal Term                   -- only value
    | Unbox  Term                   -- OK
    | Never                         -- OK
    | Head   Term                   -- head t  (requires t : Sig A) 
    | Tail   Term                   -- tail t  (requires t : Sig A)
    | Cons   Term Term              -- s :: t  (requires s : A, t : ⊛∃(Sig A))
    | Read   Chan                   -- do we need lexier here to parse chan (which at this point is a) string? 

-- relevant for `advance` and `select`
isValue :: Term -> Bool
isValue (TVar v)        = True -- x
isValue UniVal          = True -- ()
isValue Zero            = True -- 0
isValue (Suc v)         = isValue v
isValue (Lambda x t)    = isValue t 
isValue (Pair v w)      = isValue v && isValue w
isValue (Inj _ v)       = isValue v
isValue (BoxVal t)      = True -- t is term, not value so don't need to recurse
isValue _               = False -- Proj, Let, App, Case, Delay, Adv, AdvAL, Select, Unbox, Never, Out/Tail, Read