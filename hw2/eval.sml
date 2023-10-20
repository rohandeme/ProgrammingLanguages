structure Eval : sig

  val isV  : Desugared.term -> bool
  val isEqPair : (Desugared.term * Desugared.term) -> Desugared.term option
  val step : Desugared.term -> Desugared.term option
  val eval : Desugared.term -> Desugared.term list

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term	    

  val result : Desugared.term -> norm
      
end = struct

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term

  structure D = Desugared

  fun isV D.Zero = true
    | isV (D.Succ t) = isV t
    | isV (D.Pair (t1, t2)) = isV t1 andalso isV t2
    | isV _ = false

  fun isEqPair ((D.Pair (t1, t2)), (D.Pair (t3, t4))) = SOME (D.Cond ((D.Eq (t1, t3)), (D.Eq (t2, t4)), D.Zero))
    | isEqPair _ = NONE
		 
  fun step D.Zero = NONE
        | step (D.Succ t) = (case step t of SOME t' => SOME (D.Succ t') | NONE => NONE)
        | step (D.Add (D.Zero, t)) = SOME t
        | step (D.Add ((D.Succ t1), t2)) = SOME (D.Add (t1, (D.Succ t2)))
        | step (D.Add (t1, t2)) = (case step t1 of SOME t1' => SOME (D.Add (t1', t2)) | NONE => NONE)
        | step (D.Subtract (D.Zero, t)) = SOME D.Zero
        | step (D.Subtract (t, D.Zero)) = SOME t 
        | step (D.Subtract ((D.Succ t1), (D.Succ t2))) = SOME (D.Subtract (t1, t2))
        | step (D.Subtract (t1, t2)) = (case step t1 of SOME t1' => SOME (D.Subtract (t1', t2)) | NONE => (case step t2 of SOME t2' => if isV t1 then SOME (D.Subtract (t1, t2')) else NONE | NONE => NONE))
        | step (D.Less (D.Zero, D.Zero)) = SOME D.Zero
        | step (D.Less (D.Zero, (D.Succ t))) = if isV t then SOME (D.Succ D.Zero) else NONE
        | step (D.Less (v, D.Zero)) = if isV v then SOME D.Zero else NONE
        | step (D.Less ((D.Succ t1), (D.Succ t2))) = if isV t1 andalso isV t2 then SOME (D.Less (t1, t2)) else NONE
        | step (D.Less (t1, t2)) = (case step t1 of SOME t1' => SOME (D.Less (t1', t2)) | NONE => (case step t2 of SOME t2' => SOME (D.Less (t1, t2')) | NONE => NONE))
        | step (D.Eq (D.Zero, D.Zero)) = SOME (D.Succ D.Zero)
        | step (D.Eq (D.Zero, (D.Succ t))) = if isV t then SOME D.Zero else NONE
        | step (D.Eq (D.Succ t, D.Zero)) = if isV t then SOME D.Zero else NONE
        | step (D.Eq ((D.Succ v1), (D.Succ v2))) = 
                      if (isV v1) andalso (isV v2) then SOME (D.Eq (v1, v2)) 
                      else (case step v1 of SOME v1' => SOME (D.Eq(v1', v2))
                                              | NONE => if isV v1 then 
                                                    (case step v2 of SOME v2' => SOME (D.Eq(v1, v2')) 
                                                                      | NONE => NONE) else NONE)
        | step (D.Eq (t1, t2)) = (isEqPair (t1, t2))
        | step (D.Cond ((D.Succ D.Zero), t2, t3)) = SOME t2
        | step (D.Cond (D.Zero, t2, t3)) = SOME t3
        | step (D.Cond (t1, t2, t3)) = (case step t1 of SOME t1' => SOME (D.Cond (t1', t2, t3)) | NONE => NONE)
        | step (D.Pair (t1, t2)) = (case step t1 of SOME t1' => SOME (D.Pair (t1', t2)) | NONE => if isV t1 then (case step t2 of SOME t2' => SOME (D.Pair (t1, t2')) | NONE => NONE) else NONE)
        | step (D.First (D.Pair (t1, t2))) = if isV t1 then SOME t1 else (case step t1 of SOME t1' => SOME (D.First (D.Pair (t1', t2))) | NONE => NONE)
        | step (D.Second (D.Pair (t1, t2))) = if isV t2 then SOME t2 else (case step t2 of SOME t2' => SOME (D.Second (D.Pair (t1, t2'))) | NONE => NONE)
        | step (D.First t) = (case step t of SOME t' => SOME (D.First t') | NONE => NONE)
        | step (D.Second t) = (case step t of SOME t' => SOME (D.Second t') | NONE => NONE)


				    
  fun eval t =
    let
      fun lp t =
	(case step t
	   of SOME t' => t :: lp t'
	    | NONE => [t])
    in
      lp t
    end		    

  fun result terms = if isV (List.last (eval terms)) then (Value (List.last (eval terms))) else (Stuck (List.last (eval terms)))

end
