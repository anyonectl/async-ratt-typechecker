module ErrorTypes where

import Syntax

data ContextError
    = VariableNotFound Var
    | ContextIsEmpty String
    | UnstableType Var Type
    | NoTickInContext
    | OtherContextError String
    deriving (Eq, Show)

data TypeError
  = TypeMismatch            { term :: Term, expected :: Type, got :: Type }
  | ExpectedAL              { term :: Term, got :: Type }
  | ExpectedEL              { term :: Term, got :: Type }
  | ExpectedValue           Term
  | ExpectedBufferChannel   Term
  | ExpectedPushChannel     Term
  | CtxError                ContextError
  | OtherError              String
  deriving (Eq)

instance Show TypeError where
    show (TypeMismatch t e g) = 
        "Type mismatch in term " ++ show t ++ 
        ": expected type " ++ show e ++ 
        ", but got type " ++ show g
    show (ExpectedAL t g) = 
        "Type mismatch in term " ++ show t ++
        ": expected type AL _" ++ 
        ", but got type " ++ show g 
    show (ExpectedEL t g) = 
        "Type mismatch in term " ++ show t ++
        ": expected type EL _" ++ 
        ", but got type " ++ show g
    show (ExpectedValue t) = 
        "Type mismatch in term " ++ show t ++
        ": expected value term" -- term? 
    show (ExpectedBufferChannel t) = 
        "Type mismatch in term " ++ show t ++
        ": expected channel to be buffer"
    show (ExpectedPushChannel t) = 
        "Type mismatch in term " ++ show t ++
        ": expected channel to be push"
    show (CtxError err) = "Context Error: " ++ show err
    show (OtherError msg) = "Error: " ++ msg