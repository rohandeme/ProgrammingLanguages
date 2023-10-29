structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct
		  
  fun fv (ULC.Var x) = VarSet.ins(x, VarSet.empty)
    | fv (ULC.App (t1, t2)) = VarSet.union (fv t1, fv t2)
    | fv (ULC.Lam (x, t)) = VarSet.rem (x, fv t)

		  
  fun subst (x, s, ULC.Var y) = if x = y then s else ULC.Var y
  | subst (x, s, ULC.App (t1, t2)) = ULC.App (subst (x, s, t1), subst (x, s, t2))
  | subst (x, s, ULC.Lam (y, t)) = if x = y then ULC.Lam (y, t) else if VarSet.mem(y, fv s) then
      let
        val y' = Fresh.var ()
        val t' = subst (y, ULC.Var y', t)
      in
        subst (x, s, ULC.Lam(y', t'))
      end
    else
      ULC.Lam (y, subst (x, s, t))

end
