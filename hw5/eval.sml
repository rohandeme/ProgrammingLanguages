structure Eval : sig

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR
  structure S = Subst

  fun eval (L.Int n) = L.Int n
    | eval L.True = L.True
    | eval L.False = L.False
    | eval L.Unit = L.Unit
    | eval (L.Lam (x, tau, t)) = L.Lam (x, tau, t)
    | eval (L.Not t) = 
      let 
        val t' = eval t
      in 
        if t' = L.True then L.False else if t' = L.False then L.True else raise Fail "Stuck"
      end
    | eval (L.Add (t1, t2)) = 
      let 
        val t1' = eval t1
        val t2' = eval t2
      in
        (case (t1', t2')  of (L.Int n1, L.Int n2) => L.Int (n1 + n2) | _ => raise Fail "Stuck")
      end
    | eval (L.Sub (t1, t2)) = 
      let 
        val t1' = eval t1
        val t2' = eval t2
      in
        (case (t1', t2')  of (L.Int n1, L.Int n2) => L.Int (n1 - n2) | _ => raise Fail "Stuck")
      end
    | eval (L.Mul (t1, t2)) = 
      let 
        val t1' = eval t1
        val t2' = eval t2
      in
        (case (t1', t2')  of (L.Int n1, L.Int n2) => L.Int (n1 * n2) | _ => raise Fail "Stuck")
      end
    | eval (L.Eq (t1, t2)) = 
      let 
        val t1' = eval t1
        val t2' = eval t2
      in
        if t1' = t2' then L.True else L.False
      end
    | eval (L.LessThan (t1, t2)) = 
      let 
        val t1' = eval t1
        val t2' = eval t2
      in
        (case (t1', t2')  of (L.Int n1, L.Int n2) => if n1 < n2 then L.True else L.False | _ => raise Fail "Stuck")
      end
    | eval (L.App (t1, t2)) = 
      let 
        val t1' = eval t1
      in 
        (case t1' of L.Lam (x, tau, t) => eval (S.subst (x, t2, t)) | _ => raise Fail "Stuck")
      end
    | eval (L.Cond (t, t1, t2)) = 
      let 
        val t' = eval t
      in 
        if t' = L.True then eval t1 else eval t2
      end
    | eval (L.Record pairs) = L.Record (map (fn (l, t) => (l, eval t)) pairs)
    | eval (L.Select (l, t)) = 
      let 
        val t' = eval t
      in 
        (case t' of L.Record pairs => (case List.find (fn (l', t) => l = l') pairs of SOME (l', t) => t | NONE => raise Fail "Stuck") | _ => raise Fail "Stuck")
      end
    | eval (L.Fix t) = 
      let
        val t' = eval t
      in
        (case t' of L.Lam (x, tau, t) => eval (S.subst (x, L.Fix (L.Lam (x, tau, t)), t)) | _ => raise Fail "Stuck")
      end
    | eval (L.Let (x, t1, t2)) = 
      let 
        val t1' = eval t1
      in
        eval (S.subst (x, t1', t2))
      end
    | eval (L.Var x) = raise Fail "Vars should not be in the input to eval"
		 
end
