module ErrorTypes where

import Syntax
import PrettyPrinter

data ContextError
    = VariableNotFound Var
    | ContextIsEmpty String
    | UnstableType Var Type
    | NoTickInContext
    | OtherContextError String
    deriving (Eq, Show)

data TypeError
  = TypeMismatch            { term :: Term, expected :: Type, got :: Type }
  | IncompatibleType        { term :: Term, needed :: String, got :: Type }
  | TypeCheckFailed         { term :: Term, checkedType :: Type , inferredType :: Type }
  | ExpectedAL              { term :: Term, got :: Type }
  | ExpectedEL              { term :: Term, got :: Type }
  | ExpectedSig             { term :: Term, got :: Type }
  | ExpectedValue           Term
  | ExpectedBufferChannel   Term
  | ExpectedPushChannel     Term
  | MissingAnnotation       Term
  | CtxError                ContextError
  | OtherError              String
  deriving (Eq)

instance Show TypeError where
    show (TypeMismatch t e g) = 
        "Type mismatch in term " ++ show t ++ 
        ": expected type " ++ show e ++ 
        ", but got type " ++ show g

    show (IncompatibleType t e g) = 
        "Cannot check wrong type: '"
        ++ show t ++ "' must have '" ++ e ++ 
        "', but was annotated with type '" ++ show g ++ "'"

    show (TypeCheckFailed t cty ity) =
        "Check failed on term " ++ show t ++
        ": checked " ++ show cty ++
        ", but inferred " ++ show ity

    show (ExpectedAL t g) = 
        "Term " ++ show t ++
        ": expected type AL _" ++ 
        ", but got type " ++ show g 

    show (ExpectedEL t g) = 
        "Term " ++ show t ++
        ": expected type EL _" ++ 
        ", but got type " ++ show g

    show (ExpectedSig t g) =
        "Term " ++ show t ++
        ": expected type Sig a" ++
        ", but got type " ++ show g

    show (ExpectedValue t) = 
        "Term " ++ show t ++
        ": expected value"

    show (ExpectedBufferChannel t) = 
        "Term " ++ show t ++
        ": expected channel to be buffer"

    show (ExpectedPushChannel t) = 
        "Term" ++ show t ++
        ": expected channel to be push"

    show (MissingAnnotation t) =
        "Cannot infer type of " ++ show t ++ 
        ", annotate with expected type"

    show (CtxError err) = 
        "Context Error: " ++ show err

    show (OtherError msg) = 
        "Error: " ++ msg