structure CBV : sig

  val step : ULC.term -> ULC.term option

end = struct

  fun step (ULC.App (ULC.Lam (x, t), ULC.Var v2)) = SOME (Subst.subst (x, ULC.Var v2, t))
    | step (ULC.App (ULC.Lam (x, t), ULC.Lam (x2, t2))) = SOME (Subst.subst (x, ULC.Lam (x2, t2), t))
    | step (ULC.App (t1, t2)) = (case step t1 of SOME t1' => SOME (ULC.App (t1', t2)) | NONE => (case step t2 of SOME t2' => SOME (ULC.App (t1, t2')) | NONE => NONE))
    | step _ = NONE

end
