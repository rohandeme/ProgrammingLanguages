structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term

end = struct

  fun desugar (Sweetl.Var x) = ULC.Var x
    | desugar (Sweetl.App (t1, t2)) = ULC.App ((desugar t1), (desugar t2))
    | desugar (Sweetl.Lam (x, t)) = ULC.Lam (x, desugar t)
    | desugar (Sweetl.Abbr x) = ULC.Var x
    | desugar (Sweetl.Tru) = ULC.Lam ("t", ULC.Lam ("f", ULC.Var "t"))
    | desugar (Sweetl.Fls) = ULC.Lam ("t", ULC.Lam ("f", ULC.Var "f"))
    | desugar (Sweetl.ID x) = ULC.Lam (x, ULC.Var x)
    | desugar (Sweetl.Nat n) = 
        let 
          fun convert n acc = if n = 0 then acc else convert (n - 1) (ULC.App (ULC.Var "s", acc))
          val num = convert n (ULC.Var "z")
        in
          ULC.Lam ("s", ULC.Lam ("z", num))
        end
end
