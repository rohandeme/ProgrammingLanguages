structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV A.Zero = true 
        | isNV (A.Succ t) = isNV t 
        | isNV _ = false

  fun isV A.True = true 
        | isV A.False = true 
        | isV t = isNV t
		 
  fun step (A.Succ t) =
        (case step t of
          SOME t' => SOME (A.Succ t')
        | NONE => NONE)
    | step (A.Pred A.Zero) = SOME A.Zero
    | step (A.Pred t) = if isNV t then (case t of A.Succ nv1 => SOME nv1 | A.Zero => raise Fail "AYO" | _ => raise Fail "AYOOOOO") else (case step t of SOME t' => SOME (A.Pred t') | NONE => NONE)
    | step (A.Cond (A.True, t2, _)) = SOME t2
    | step (A.Cond (A.False, _, t3)) = SOME t3
    | step (A.Cond (t1, t2, t3)) =
        (case step t1 of
          SOME t1' => SOME (A.Cond (t1', t2, t3))
        | NONE => NONE)
    | step (A.And (A.True, t2)) = SOME t2
    | step (A.And (A.False, _)) = SOME A.False
    | step (A.And (t1, t2)) =
        (case step t1 of
          SOME t1' => SOME (A.And (t1', t2))
        | NONE => NONE)
    | step (A.Or (A.True, _)) = SOME A.True
    | step (A.Or (A.False, t2)) = SOME t2
    | step (A.Or (t1, t2)) =
        (case step t1 of
          SOME t1' => SOME (A.Or (t1', t2))
        | NONE => NONE)
    | step (A.Add (A.Zero, t)) = SOME t
    | step (A.Add ((A.Succ t1), t2)) = if isNV t1 then SOME (A.Add (t1, (A.Succ t2))) else (case step t1 of SOME t1' => SOME (A.Add (t1', A.Succ t2)) | NONE => NONE)
    | step (A.Add (t1, t2)) = (case step t1 of SOME t1' => SOME (A.Add (t1', t2)) | None => None)
    | step (A.Subtract (A.Zero, t)) = if isNV t then SOME A.Zero else (case step t of SOME t' => SOME (A.Subtract (A.Zero, t')) | NONE => NONE)
    | step (A.Subtract (t, A.Zero)) = if isNV t then SOME t else (case step t of SOME t' => SOME (A.Subtract (t', A.Zero)) | NONE => NONE)
    | step (A.Subtract (A.Succ t1, A.Succ t2)) = if isNV t1 andalso isNV t2 then SOME (A.Subtract (t1, t2)) else (case step t1 of SOME t1' => SOME (A.Subtract (t1', t2)) | NONE => (case step t2 of SOME t2' => SOME (A.Subtract (t1, t2')) | NONE => NONE))
    | step (A.Subtract (t1, t2)) = (case step t1 of SOME t1' => SOME (A.Subtract(t1', t2)) | NONE => (case step t2 of SOME t2' => if isV t1 then SOME (A.Subtract (t1, t2')) else NONE | NONE => NONE))
    | step (A.Less (A.Zero, A.Zero)) = SOME A.False
    | step (A.Less (A.Zero, A.Succ t)) = if isNV t then SOME A.True else (case step t of SOME t' => SOME (A.Less (A.Zero, t')) | NONE => NONE)
    | step (A.Less (t, A.Zero)) = if isNV t then SOME A.False else (case step t of SOME t' => SOME (A.Less (t', A.Zero)) | NONE => NONE)
    | step (A.Less ((A.Succ t1), (A.Succ t2))) = if isNV t1 andalso isNV t2 then SOME (A.Less (t1, t2)) else (case step t1 of SOME t1' => SOME (A.Less ((A.Succ t1'), (A.Succ t2))) | NONE => (case step t2 of SOME t2' => SOME (A.Less ((A.Succ t1), (A.Succ t2'))) | NONE => NONE))
    | step (A.Less (t1, t2)) = (case step t1 of SOME t1' => SOME (A.Less(t1', t2)) | NONE => (case step t2 of SOME t2' => if isV t1 then SOME (A.Less(t1, t2')) else NONE | NONE => NONE))
    | step (A.Greater (A.Zero, t)) = if isNV t then SOME A.False else (case step t of SOME t' => SOME (A.Greater (A.Zero, t')) | NONE => NONE)
    | step (A.Greater (A.Succ t, A.Zero)) = if isNV t then SOME A.True else (case step t of SOME t' => SOME (A.Greater (t', A.Zero)) | NONE => NONE) 
    | step (A.Greater (A.Succ t1, A.Succ t2)) = if isNV t1 andalso isNV t2 then SOME (A.Greater (t1, t2)) else (case step t1 of SOME t1' => SOME (A.Greater ((A.Succ t1'), (A.Succ t2))) | NONE => (case step t2 of SOME t2' => SOME (A.Greater ((A.Succ t1), (A.Succ t2'))) | NONE => NONE))
    | step (A.Greater (t1, t2)) = (case step t1 of SOME t1' => SOME (A.Greater (t1', t2)) | NONE => (case step t2 of SOME t2' => if isV t1 then SOME (A.Greater(t1, t2')) else NONE | NONE => NONE))
    | step _ = NONE


  fun eval term = (case step term of
                     SOME term2 => term :: eval term2
                   | NONE => [term])
	 
end
