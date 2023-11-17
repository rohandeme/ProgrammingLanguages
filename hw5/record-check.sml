structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)

  val check : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  fun check term =
    let
      fun distinctLabels labels =
        case labels of
          [] => ()
        | (label, _) :: rest =>
            if List.exists (fn (l, _) => l = label) rest then
              raise Fail "Labels in record are not distinct."
            else
              distinctLabels rest
      fun checkTerm t =
        case t of
          L.Record pairs =>
            (distinctLabels pairs; L.Record (map (fn (l, t') => (l, checkTerm t')) pairs))
        | L.Select (label, t') => L.Select (label, checkTerm t')
        | L.Cond (t1, t2, t3) => L.Cond (checkTerm t1, checkTerm t2, checkTerm t3)
        | L.Add (t1, t2) => L.Add (checkTerm t1, checkTerm t2)
        | L.Sub (t1, t2) => L.Sub (checkTerm t1, checkTerm t2)
        | L.Mul (t1, t2) => L.Mul (checkTerm t1, checkTerm t2)
        | L.Eq (t1, t2) => L.Eq (checkTerm t1, checkTerm t2)
        | L.LessThan (t1, t2) => L.LessThan (checkTerm t1, checkTerm t2)
        | L.Not t' => L.Not (checkTerm t')
        | L.App (t1, t2) => L.App (checkTerm t1, checkTerm t2)
        | L.Fix t' => L.Fix (checkTerm t')
        | L.Let (x, t1, t2) => L.Let (x, checkTerm t1, checkTerm t2)
        | L.Lam (x, tau, t') => L.Lam (x, tau, checkTerm t')
        | _ => t
    in
      checkTerm term
    end

end
