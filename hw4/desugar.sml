structure Desugar : sig

  val desugar : Sugary.term -> ULC.term

end = struct

  structure S = Sugary
  structure U = ULC

  fun desugar (S.Nat n) = 
        let 
          fun convert n acc = if n = 0 then acc else convert (n - 1) (U.App (ULC.Var "s", acc))
          val num = convert n (U.Var "z")
        in
          U.Lam ("s", U.Lam ("z", num))
        end
    | desugar S.True = U.Lam ("t", U.Lam ("f", U.Var "t"))
    | desugar S.False = U.Lam ("t", U.Lam ("f", U.Var "f"))
    | desugar S.Unit = U.Lam ("s", U.Lam ("z", U.Var "z"))
    | desugar (S.Add (t1, t2)) = U.App (U.App (U.Lam ("m", ULC.Lam ("n", ULC.Lam ("s", ULC.Lam ("z", ULC.App (ULC.App (ULC.Var "m", ULC.Var "s"), ULC.App (ULC.App (ULC.Var "n", ULC.Var "s"), ULC.Var "z")))))), desugar t1), desugar t2)
    | desugar (S.Subtract (t1, t2)) = 
      let
        val zz = desugar (S.Pair (S.Nat 0, S.Nat 0))
        val pair = ULC.Lam ("f", ULC.Lam ("s", ULC.Lam ("b", ULC.App (ULC.App (ULC.Var "b", ULC.Var "f"), ULC.Var "s"))))
        val plus = U.Lam ("m", ULC.Lam ("n", ULC.Lam ("s", ULC.Lam ("z", ULC.App (ULC.App (ULC.Var "m", ULC.Var "s"), ULC.App (ULC.App (ULC.Var "n", ULC.Var "s"), ULC.Var "z"))))))
        val fst = ULC.Lam ("p", ULC.App (U.Var "p", desugar S.True))
        val snd = ULC.Lam ("p", ULC.App (U.Var "p", desugar S.False))
        val add = U.Lam ("m", ULC.Lam ("n", ULC.Lam ("s", ULC.Lam ("z", ULC.App (ULC.App (ULC.Var "m", ULC.Var "s"), ULC.App (ULC.App (ULC.Var "n", ULC.Var "s"), ULC.Var "z"))))))
        val ss = U.Lam ("p", U.App (U.App (pair, U.App (snd, U.Var "p")), U.App (U.App (add, desugar (S.Nat 1)), U.App (snd, U.Var "p"))))
        val prd = U.Lam ("m", U.App (fst, U.App (U.App (U.Var "m", ss), zz)))
        val not = U.Lam ("x", U.App (U.App (U.Var "x", desugar S.False), desugar S.True))
        val subtract = U.Lam ("m", U.Lam ("n", U.App (U.App (U.Var "n", prd), U.Var "m")))
      in
        U.App (U.App (subtract, desugar t1), desugar t2)
      end
    | desugar (S.Mul (t1, t2)) = U.App (U.App (U.Lam ("m", U.Lam ("n", U.Lam ("s", U.App (U.Var "m", U.App (U.Var "n", U.Var "s"))))), desugar t1), desugar t2)
    | desugar (S.Pow (t1, t2)) = 
      let 
        val times = U.Lam ("m", U.Lam ("n", U.Lam ("s", U.App (U.Var "m", U.App (U.Var "n", U.Var "s")))))
      in
        U.App (U.App (U.Lam ("m", U.Lam ("n", U.App (U.App (U.Var "n", U.App (times, U.Var "m")), desugar (S.Nat 1)))), desugar t1), desugar t2)
      end
    | desugar (S.Less (t1, t2)) = desugar (S.Greater (t2, t1))
    | desugar (S.Greater (t1, t2)) = 
      let 
        val fls = U.Lam ("t", U.Lam ("f", U.Var "f"))
        val tru = U.Lam ("t", U.Lam ("f", U.Var "t"))
        val iszro = U.Lam ("m", U.App (U.App (U.Var "m", U.Lam ("x", fls)), tru))
        val zz = desugar (S.Pair (S.Nat 0, S.Nat 0))
        val pair = ULC.Lam ("f", ULC.Lam ("s", ULC.Lam ("b", ULC.App (ULC.App (ULC.Var "b", ULC.Var "f"), ULC.Var "s"))))
        val plus = U.Lam ("m", ULC.Lam ("n", ULC.Lam ("s", ULC.Lam ("z", ULC.App (ULC.App (ULC.Var "m", ULC.Var "s"), ULC.App (ULC.App (ULC.Var "n", ULC.Var "s"), ULC.Var "z"))))))
        val fst = ULC.Lam ("p", ULC.App (U.Var "p", desugar S.True))
        val snd = ULC.Lam ("p", ULC.App (U.Var "p", desugar S.False))
        val add = U.Lam ("m", ULC.Lam ("n", ULC.Lam ("s", ULC.Lam ("z", ULC.App (ULC.App (ULC.Var "m", ULC.Var "s"), ULC.App (ULC.App (ULC.Var "n", ULC.Var "s"), ULC.Var "z"))))))
        val ss = U.Lam ("p", U.App (U.App (pair, U.App (snd, U.Var "p")), U.App (U.App (add, desugar (S.Nat 1)), U.App (snd, U.Var "p"))))
        val prd = U.Lam ("m", U.App (fst, U.App (U.App (U.Var "m", ss), zz)))
        val and1 = U.Lam ("b", U.Lam ("c", U.App (U.App (U.Var "b", U.Var "c"), fls)))
        val not = U.Lam ("x", U.App (U.App (U.Var "x", desugar S.False), desugar S.True))
        val greater = U.Lam ("m", U.Lam("n", U.App (U.App (and1, U.App (iszro, U.App (U.App (U.Var "m", prd), U.Var "n"))), U.App (not, U.App (iszro, U.App (U.App (U.Var "n", prd), U.Var "m"))))))
      in  
         U.App (U.App (greater, desugar t1), desugar t2)
      end
    | desugar (S.LessEq (t1, t2)) = desugar (S.GreaterEq (t2, t1))
    | desugar (S.GreaterEq (t1, t2)) = desugar (S.Or (S.Greater (t1, t2), S.Eq (t1, t2)))
    | desugar (S.Not t) = U.App (U.Lam ("x", U.App (U.App (U.Var "x", desugar S.False), desugar S.True)), desugar t)
    | desugar (S.And (t1, t2)) = U.App (U.App (U.Lam ("b", U.Lam ("c", U.App (U.App (U.Var "b", U.Var "c"), desugar S.False))), desugar t1), desugar t2)
    | desugar (S.Or (t1, t2)) = U.App (U.App (U.Lam ("x", U.Lam ("y", U.App (U.App (U.Var "x", U.Var "x"), U.Var "y"))), desugar t1), desugar t2)
    | desugar (S.Xor (t1, t2)) = U.App (U.App (U.Lam ("x", U.Lam ("y", U.App (U.App (U.Var "x", U.App (U.App (U.Var "y", desugar S.False), desugar S.True)), U.App (U.App (U.Var "y", desugar S.True), desugar S.False)))), desugar t1), desugar t2)
    | desugar (S.Cond (t1, t2, t3)) = U.App (U.App (U.App (U.Lam ("x", U.Lam ("y", U.Lam ("z", U.App (U.App (U.Var "x", U.Var "y"), U.Var "z")))), desugar t1), desugar t2), desugar t3)
    | desugar (S.Eq (t1, t2)) =
      let 
        val fls = U.Lam ("t", U.Lam ("f", U.Var "f"))
        val tru = U.Lam ("t", U.Lam ("f", U.Var "t"))
        val iszro = U.Lam ("m", U.App (U.App (U.Var "m", U.Lam ("x", fls)), tru))
        val zz = desugar (S.Pair (S.Nat 0, S.Nat 0))
        val pair = ULC.Lam ("f", ULC.Lam ("s", ULC.Lam ("b", ULC.App (ULC.App (ULC.Var "b", ULC.Var "f"), ULC.Var "s"))))
        val plus = U.Lam ("m", ULC.Lam ("n", ULC.Lam ("s", ULC.Lam ("z", ULC.App (ULC.App (ULC.Var "m", ULC.Var "s"), ULC.App (ULC.App (ULC.Var "n", ULC.Var "s"), ULC.Var "z"))))))
        val fst = ULC.Lam ("p", ULC.App (U.Var "p", desugar S.True))
        val snd = ULC.Lam ("p", ULC.App (U.Var "p", desugar S.False))
        val add = U.Lam ("m", ULC.Lam ("n", ULC.Lam ("s", ULC.Lam ("z", ULC.App (ULC.App (ULC.Var "m", ULC.Var "s"), ULC.App (ULC.App (ULC.Var "n", ULC.Var "s"), ULC.Var "z"))))))
        val ss = U.Lam ("p", U.App (U.App (pair, U.App (snd, U.Var "p")), U.App (U.App (add, desugar (S.Nat 1)), U.App (snd, U.Var "p"))))
        val prd = U.Lam ("m", U.App (fst, U.App (U.App (U.Var "m", ss), zz)))
        val and1 = U.Lam ("b", U.Lam ("c", U.App (U.App (U.Var "b", U.Var "c"), desugar S.False)))
        val equal = U.Lam ("m", U.Lam("n", U.App (U.App (and1, U.App (iszro, U.App (U.App (U.Var "m", prd), U.Var "n"))), U.App (iszro, U.App (U.App (U.Var "n", prd), U.Var "m")))))
      in  
         U.App (U.App (equal, desugar t1), desugar t2)
      end
    | desugar (S.Pair (t1, t2)) = ULC.App (ULC.App (ULC.Lam ("f", ULC.Lam ("s", ULC.Lam ("b", ULC.App (ULC.App (ULC.Var "b", ULC.Var "f"), ULC.Var "s")))), desugar t1), desugar t2)
    | desugar (S.First t) = U.App (ULC.Lam ("p", ULC.App (U.Var "p", desugar S.True)), desugar t)
    | desugar (S.Second t) = U.App (ULC.Lam ("p", ULC.App (U.Var "p", desugar S.False)), desugar t)
    | desugar (S.Var x) = U.Var x
    | desugar (S.Let (x, t1, t2)) = U.App (U.Lam (x, desugar t2), desugar t1)
    
end
