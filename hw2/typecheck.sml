structure TypeCheck : sig

  val typeof : Sugary.term -> Type.typ

end = struct

  structure S = Sugary
  structure T = Type
  
  fun typeof (S.Nat _) = T.Nat
        | typeof S.True = T.Bool
        | typeof S.False = T.Bool
        | typeof S.Unit = T.Unit
        | typeof (S.Add (t1, t2)) = (case (typeof t1, typeof t2) of (T.Nat, T.Nat) => T.Nat | _ => raise Fail "Type Mismatch")
        | typeof (S.Subtract (t1, t2)) = (case (typeof t1, typeof t2) of (T.Nat, T.Nat) => T.Nat | _ => raise Fail "Type Mismatch")
        | typeof (S.Less (t1, t2)) = (case (typeof t1, typeof t2) of (T.Nat, T.Nat) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.Greater (t1, t2)) = (case (typeof t1, typeof t2) of (T.Nat, T.Nat) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.GreaterEq (t1, t2)) = (case (typeof t1, typeof t2) of (T.Nat, T.Nat) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.LessEq (t1, t2)) = (case (typeof t1, typeof t2) of (T.Nat, T.Nat) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.Eq (t1, t2)) = (case (typeof t1, typeof t2) of (T.Nat, T.Nat) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.And (t1, t2)) = (case (typeof t1, typeof t2) of (T.Bool, T.Bool) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.Or (t1, t2)) = (case (typeof t1, typeof t2) of (T.Bool, T.Bool) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.Xor (t1, t2)) = (case (typeof t1, typeof t2) of (T.Bool, T.Bool) => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.Not t) = (case typeof t of T.Bool => T.Bool | _ => raise Fail "Type Mismatch")
        | typeof (S.Cond (t1, t2, t3)) = (case (typeof t1, typeof t2, typeof t3) of (T.Bool, T.Bool, T.Bool) => T.Bool | (T.Bool, T.Nat, T.Nat) => T.Bool | (T.Bool, T.Unit, T.Unit) => T.Bool| _ => raise Fail "Type Mismatch")
        | typeof (S.Pair (t1, t2)) = (T.Product (typeof t1, typeof t2))
        | typeof (S.First t) = (case typeof t of (T.Product (ty1, _)) => ty1 | _ => raise Fail "Type Mismatch")
        | typeof (S.Second t) = (case typeof t of (T.Product (_, ty2)) => ty2 | _ => raise Fail "Type Mismatch")
end
