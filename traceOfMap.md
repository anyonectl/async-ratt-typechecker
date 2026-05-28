-- HAND-TRACING A TYPE CHECK OF MAP

-- mapType :: Type
-- mapType = Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat))

infer anno mapterm maptype
  check (fix "r" lam) maptype   -- check mapterm maptype
    add "r" : AL maptype
    check (lam "f" lam) (Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat)))
      add "f" (Box (Fun Nat Nat))
      check (lam "s" let) (Fun (Sig Nat) (Sig Nat))
        add "s" (Sig Nat)
        check (let "x" head let) (Sig Nat)
          infer (head "s")
            infer "s"
              lookup "s" -> Sig Nat
            ty = Sig Nat
          ty1 = Nat
          add "x" Nat
          check (let "xs" tail cons) (Sig Nat)
            infer (tail "s")
              infer "s"
                lookup "s" -> Sig Nat
              ty = Sig Nat
            ty1 = EL (Sig Nat)
            add "xs" (EL (Sig Nat))
            check (cons app delay) (Sig Nat)
              check (app (unbox "f") "x") (Nat) -- (default)
                infer (app (unbox "f") "x")     
                  infer (unbox "f")
                    infer "f"
                      lookup "f" -> Box (Fun Nat Nat)
                    ty = Box (Fun Nat Nat)
                  ty = Fun Nat Nat
                  check "x" Nat                 -- (default)
                    infer "x"
                    lookup "x" -> Nat
                    Nat == Nat
                  _ = Nat
                Nat == Nat
              _ = Nat
              check (delay (cl("xs")) (app (app adv))) (EL (Sig Nat))
                addTick cl("xs")
                check (app ((app (advA "r") "f") (adv "xs"))) (Sig Nat) -- default
                  infer (app ((app (advA "r") "f") (adv "xs")))
                    infer (app (advA "r") "f")
                      infer (advA "r")
                        infer "r"   -- (in ctx before tick)
                          lookup "r" -> AL maptype
                        ty = AL maptype
                      ty = maptype  -- (Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat)))
                      check "f" (Box (Fun Nat Nat))
                        infer "f"
                          lookup "f" -> (Box (Fun Nat Nat)) -- we are in a ctx after a tick, so the only reason this works is that Box is a stable type!
                      _ = Box (Fun Nat Nat)
                    ty = Fun (Sig Nat) (Sig Nat)
                    check (adv "xs") (Sig Nat)  -- default
                      infer (adv "xs")
                        infer "xs"  -- in ctx before tick
                          lookup "xs" -> (EL (Sig Nat))
                        tyBeforeTick = EL (Sig Nat)
                      Sig Nat == Sig Nat
                  Sig Nat == Sig Nat
                _ = Sig Nat
              _ = EL (Sig Nat)
            _ = Sig Nat
          _ = Sig Nat
        _ = Sig Nat
      _ = Fun (Sig Nat) (Sig Nat)
    _ = Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat))
  _ = Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat))
Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat))   -- maptype (final returned result type)

    
      
                      





                        

            