module ErrorTypes where

import Syntax

data ContextError
    = VariableNotFound Var
    | ContextIsEmpty String
    | UnstableType Var Type
    | NoTickInContext
    | OtherContextError String
    deriving (Eq, Show)

data Expected
    = EEL | EAL | EProd | ESum | ESig | EBox | EFun | ENat
    | EValue | EBufChan | EPushChan
    deriving (Eq)

data TypeError
  = TypeCheckFailed         { term :: Term, checked :: Type, inferred :: Type }
  | IncompatibleType        { term :: Term, expected :: Expected, got :: Type }
  | IncompatibleChan        { term :: Term, expected :: Expected, gotC :: Chan }
  | IncompatibleTerm        { term :: Term, expected :: Expected, gotT :: Term }
  | MissingAnnotation       Term
  | CtxError                ContextError
  | OtherError              String
  deriving (Eq)

instance Show Expected where
    show EEL = "sometime-later type (EL)"
    show EAL = "anytime-later type (AL)"
    show EProd = "product type (Prod)"
    show ESum = "sum type (Sum)"
    show ESig = "signal type (Sig)"
    show EBox = "box type (Box)"
    show EFun = "function type (Fun)"
    show ENat = "nat (Nat)"
    show EValue = "value term (v)"
    show EBufChan = "buffer channel (b, pb)"
    show EPushChan = "push channel (p, pb)"

instance Show TypeError where
    show (TypeCheckFailed t cty ity) =
        "TypeCheckFailed: on term '" ++ show t ++
        "': checked '" ++ show cty ++
        "', but inferred '" ++ show ity ++ "'"
    
    show (IncompatibleType t e g) = 
        "IncompatibleType: '"
        ++ show t ++ "' must have '" ++ show e ++ 
        "', but got type '" ++ show g ++ "'"

    show (IncompatibleTerm t e g) =
        "IncompatibleTerm: '" ++ show t ++
        "' expected '" ++ show e ++
        "', but got '" ++ show g ++ "'"

    show (IncompatibleChan t e g) = 
        "IncompatibleChan: '" ++ show t ++
        "' expected '" ++ show e ++
        "', but got '" ++ show g ++ "'"

    show (MissingAnnotation t) =
        "MissingAnnotation: Cannot infer type of " ++ show t ++ 
        ", annotate with expected type"

    show (CtxError err) = 
        "ContextError: " ++ show err

    show (OtherError msg) = 
        "Error: " ++ msg