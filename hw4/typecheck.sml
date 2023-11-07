structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

  structure S = Sugary
  structure T = Type

  fun typeof (env, S.Nat n) = T.Nat
    | typeof (env, S.True) = T.Bool
    | typeof (env, S.False) = T.Bool
    | typeof (env, S.Unit) = T.Unit
    | typeof (env, S.Add (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Nat else raise Fail "Improper Types for Addition"
      end
    | typeof (env, S.Subtract (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Nat else raise Fail "Improper Types for Subtraction"
      end
    | typeof (env, S.Mul (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Nat else raise Fail "Improper Types for Multiplication"
      end
    | typeof (env, S.Pow (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Nat else raise Fail "Improper Types for Exponentiation"
      end
    | typeof (env, S.Less (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Bool else raise Fail "Improper Types for Comparison"
      end
    | typeof (env, S.Greater (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Bool else raise Fail "Improper Types for Comparison"
      end
    | typeof (env, S.GreaterEq (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Bool else raise Fail "Improper Types for Comparison"
      end
    | typeof (env, S.LessEq (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Nat andalso ty2 = T.Nat then T.Bool else raise Fail "Improper Types for Comparison"
      end
    | typeof (env, S.Not t) = if typeof (env, t) = T.Bool then T.Bool else raise Fail "Improper Types for Not"
    | typeof (env, S.And (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Bool andalso ty2 = T.Bool then T.Bool else raise Fail "Improper Types for And"
      end
    | typeof (env, S.Or (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Bool andalso ty2 = T.Bool then T.Bool else raise Fail "Improper Types for Or"
      end
    | typeof (env, S.Xor (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty1 = T.Bool andalso ty2 = T.Bool then T.Bool else raise Fail "Improper Types for Xor"
      end
    | typeof (env, S.Cond (t1, t2, t3)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
        val ty3 = typeof (env, t3)
      in
        if ty1 = T.Bool andalso ty2 = ty3 then ty2 else raise Fail "Improper Types for Cond"
      end
    | typeof (env, S.Eq (t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val ty2 = typeof (env, t2)
      in
        if ty2 = ty1 then T.Bool else raise Fail "Improper Types for Eq"
      end
    | typeof (env, S.Pair (t1, t2)) = T.Product (typeof (env, t1), typeof (env, t2))
    | typeof (env, S.First t) = (case typeof (env, t) of T.Product (t1, t2) => t1 | _ => raise Fail "Improper Types For First")
    | typeof (env, S.Second t) = (case typeof (env, t) of T.Product (t1, t2) => t2 | _ => raise Fail "Improper Types For First")
    | typeof (env, S.Var s) = (case TypeEnv.lookup (env, s) of SOME t => t | NONE => raise Fail "Var not in Type Env")
    | typeof (env, S.Let (x, t1, t2)) = 
      let
        val ty1 = typeof (env, t1)
        val env2 = TypeEnv.extend (env, x, ty1)
      in 
        typeof (env2, t2)
      end

end
