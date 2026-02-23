module Syntax where

data ClockExpr = Clock Term
               | SqUnion ClockExpr ClockExpr

data Type = Unit                    -- Like the unit type in F#
          | Nat                     -- should not take an Int because it's a TYPE
          | Prod Type Type          -- A x B is a pair (2-tuple)
          | Sum Type Type           -- sum type / 'Either'  (Louise: read up on Either, talk to Sofie) - taking the discriminated union of two types
          | Fun Type Type           -- this is a (function) type 
          | EL Type                 -- delayed computation on a local clock (exists later / sometime later)
          | AL Type                 -- delay on the global clock (anytime later)
          | Box Type                -- Stable modal operator, pronounced 'box'

data Term = Const                   -- but what does const (val) take... an integer? some other type? 
          | Var String              -- variable                     
          | Suc Term
          | Pair Term Term
          | In                      -- help, why in-i?
          | Pi                      -- help
          | Let String Term Term    -- how: need a type to represent variables. But how to refer to 'var'?? Just string?
          | App Term Term           -- function application!
          | CaseIn                  -- pattern matching
          | Delay ClockExpr Term    -- correct
          | Adv Term                -- correct
          | AdvAL Term              -- correct
          | Select Term Term
          | Unbox Term              -- correct
          | Never                   -- correct
          | Read                    -- k??