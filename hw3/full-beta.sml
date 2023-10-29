structure FullBeta : sig

  val step : ULC.term -> ULC.term option

end = struct

  fun step (ULC.App (ULC.Lam (x, t1), t2)) = SOME (Subst.subst (x, t2, t1))
    | step (ULC.App (t1, t2)) = (case step t1 of SOME t1' => SOME (ULC.App (t1', t2)) | NONE => (case step t2 of SOME t2' => SOME (ULC.App (t1, t2')) | NONE => NONE))
    | step (ULC.Lam (x, t1)) = (case step t1 of SOME t1' => SOME (ULC.Lam (x, t1')) | NONE => NONE)
    | step _ = NONE

end
