structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST
  
  fun next [] = NONE
      | next (T.Z :: rest) = SOME (A.Zero, rest)
      | next (T.T :: rest) = SOME (A.True, rest)
      | next (T.F :: rest) = SOME (A.False, rest)
      | next (T.P :: rest) = (case next rest of 
                                SOME (r, r2) => SOME (A.Pred r, r2)
                                | NONE => raise Fail "No Pred Arg")
      | next (T.S :: rest) = (case next rest of 
                                SOME (r, r2) => SOME (A.Succ r, r2)
                                | NONE => raise Fail "No Succ Arg")
      | next (T.LBrack :: rest) =
        (case next rest of
           SOME (r1, r1') =>
               (case r1' of
                  T.Plus :: r2' =>
                      (case next r2' of
                         SOME (r2, r3') =>
                             (case r3' of
                                T.RBrack :: r4' => SOME (A.Add (r1, r2), r4')
                              | _ => raise Fail "Invalid Expression")
                       | NONE => raise Fail "Invalid Expression")
                | T.Minus :: r2' =>
                      (case next r2' of
                         SOME (r2, r3') =>
                             (case r3' of
                                T.RBrack :: r4' => SOME (A.Subtract (r1, r2), r4')
                              | _ => raise Fail "Invalid Expression")
                       | NONE => raise Fail "Invalid Expression")
                | T.LessThan :: r2' =>
                      (case next r2' of
                         SOME (r2, r3') =>
                             (case r3' of
                                T.RBrack :: r4' => SOME (A.Less (r1, r2), r4')
                              | _ => raise Fail "Invalid Expression")
                       | NONE => NONE)
                | T.GreaterThan :: r2' =>
                      (case next r2' of
                         SOME (r2, r3') =>
                             (case r3' of
                                T.RBrack :: r4' => SOME (A.Greater (r1, r2), r4')
                              | _ => raise Fail "Invalid Expression")
                       | NONE => raise Fail "Invalid Expression")
                | T.DoubleAmpersand :: r2' =>
                      (case next r2' of
                         SOME (r2, r3') =>
                             (case r3' of
                                T.RBrack :: r4' => SOME (A.And (r1, r2), r4')
                              | _ => raise Fail "Invalid Expression")
                       | NONE => raise Fail "Invalid Expression")
                | T.DoublePipe :: r2' =>
                      (case next r2' of
                         SOME (r2, r3') =>
                             (case r3' of
                                T.RBrack :: r4' => SOME (A.Or (r1, r2), r4')
                              | _ => raise Fail "Invalid Expression")
                       | NONE => raise Fail "Invalid Expression")
                | T.QuestionMark :: r2' =>
                      (case next r2' of
                         SOME (r2, r3') =>
                             (case r3' of
                                T.Colon :: r4' =>
                                    (case next r4' of
                                       SOME (r3, r5') =>
                                           (case r5' of
                                              T.RBrack :: r6' => SOME (A.Cond (r1, r2, r3), r6')
                                            | _ => raise Fail "Invalid Expression")
                                     | _ => raise Fail "Invalid Expression")
                              | _ => raise Fail "Invalid Expression")
                | _ => raise Fail "Invalid Expression")
           | _ => raise Fail "Invalid Expression") 
        | _ => raise Fail "Invalid Expression")
    | next _ = raise Fail "Invalid Expression"

  fun parse tokens =
    case next tokens of
        SOME (term, []) => term
      | _ => raise Fail "Parse error: Unable to parse the entire input"
		     
end

