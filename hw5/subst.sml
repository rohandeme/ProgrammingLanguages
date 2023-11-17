structure Subst : sig

  val subst : string * L23RR.term * L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

(* note: We will do without VarSet/FV this time. *)
(* This is because free variables are effectively banished by the typechecker. *)
(* That is, we shouldn't have them when we get to evaluation. *)
		  
  fun subst (x, s, L.Var y) = if x = y then s else L.Var y
    | subst (x, s, L.Lam (y, tau, t)) = if x = y then L.Lam (y, tau, t) else L.Lam (y, tau, subst (x, s, t))
    | subst (x, s, L.App (t1, t2)) = L.App (subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.Let (y, t1, t2)) = L.Let (y, subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.Fix t) = L.Fix (subst (x, s, t))
    | subst (x, s, L.Record pairs) = L.Record (map (fn (l, t) => (l, subst (x, s, t))) pairs)
    | subst (x, s, L.Select (l, t)) = L.Select (l, subst (x, s, t))
    | subst (x, s, L.Cond (t, t1, t2)) = L.Cond (subst (x, s, t), subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.Add (t1, t2)) = L.Add (subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.Sub (t1, t2)) = L.Sub (subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.Mul (t1, t2)) = L.Mul (subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.Eq (t1, t2)) = L.Eq (subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.LessThan (t1, t2)) = L.LessThan (subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, L.Not t) = L.Not (subst (x, s, t))
    | subst (x, s, t) = t

end
