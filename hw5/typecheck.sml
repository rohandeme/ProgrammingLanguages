structure TypeCheck : sig

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  val typeof : L23RR.term -> Type.typ
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv
		  
  fun subty (T.Int, T.Int) = true
    | subty (T.Bool, T.Bool) = true
    | subty (T.Unit, T.Unit) = true
    | subty (T.Function (t1, t2), T.Function (t1', t2')) = subty (t1', t1) andalso subty (t2, t2')
    | subty (T.Record typePairs1, T.Record typePairs2) =
        let
          fun findPairWithLabel (label, pairs) =
            case pairs of
              [] => NONE
            | (l, t) :: rest => if l = label then SOME t else findPairWithLabel(label, rest)
          
          fun recordSubtype typePairs1 typePairs2 =
            let
              val width = List.all (fn (label, _) => List.exists (fn (l, _) => l = label) typePairs1) typePairs2
              val depth = List.all (fn (label, typ2) =>
                case findPairWithLabel(label, typePairs1) of
                  SOME typ1 => subty (typ1, typ2)
                | NONE => false
              ) typePairs2
            in
              width andalso depth
            end
        in
          recordSubtype typePairs1 typePairs2
        end
    | subty _ = false

  fun commonSupertype (t1, t2) = if subty (t1, t2) then SOME t2 else if subty (t2, t1) then SOME t1 else NONE

  fun typeofenv (env, term) = 
    case term of 
      L.Int n => T.Int
    | L.True => T.Bool
    | L.False => T.Bool
    | L.Unit => T.Unit
    | L.Var x =>  (case E.lookup (env, x) of SOME t => t | NONE => raise Fail "Unbound variable")
    | L.Lam (x, tau, t) => T.Function (tau, typeofenv (E.extend (env, x, tau), t))
    | L.App (t1, t2) => 
      let
        val t1Type = typeofenv (env, t1)
        val t2Type = typeofenv (env, t2)
      in
        (case t1Type of
          T.Function (t1ArgType, t1RetType) => if t1ArgType = t2Type then t1RetType else raise Fail "Application of non-matching types"
        | _ => raise Fail "Application of non-function"
        )
      end
    | L.Let (x, t1, t2) => typeofenv (E.extend (env, x, typeofenv (env, t1)), t2)
    | L.Fix t =>
      let 
        val tType = typeofenv (env, t)
      in 
        (case tType of
          T.Function (tArgType, tRetType) => if tArgType = tRetType then tRetType else raise Fail "Fix of non-matching types"
        | _ => raise Fail "Fix of non-function"
        )
      end
    | L.Add (t1, t2) => if typeofenv (env, t1) = T.Int andalso typeofenv (env, t2) = T.Int then T.Int else raise Fail "Addition of non-integers"
    | L.Sub (t1, t2) => if typeofenv (env, t1) = T.Int andalso typeofenv (env, t2) = T.Int then T.Int else raise Fail "Subtraction of non-integers"
    | L.Mul (t1, t2) => if typeofenv (env, t1) = T.Int andalso typeofenv (env, t2) = T.Int then T.Int else raise Fail "Multiplication of non-integers"
    | L.Eq (t1, t2) => if typeofenv (env, t1) = typeofenv (env, t2) then T.Bool else raise Fail "Equality of non-matching types"
    | L.LessThan (t1, t2) => if typeofenv (env, t1) = T.Int andalso typeofenv (env, t2) = T.Int then T.Bool else raise Fail "Less than of non-integers"
    | L.Not t => if typeofenv (env, t) = T.Bool then T.Bool else raise Fail "Not of non-boolean"
    | L.Record typePairs => T.Record (List.map (fn (label, term) => (label, typeofenv (env, term))) typePairs)
    | L.Cond (t, t1, t2) => 
      let
        val tType = typeofenv (env, t)
        val t1Type = typeofenv (env, t1)
        val t2Type = typeofenv (env, t2)
        val commonType = commonSupertype (t1Type, t2Type)
      in
        if tType = T.Bool andalso Option.isSome commonType then Option.valOf commonType else raise Fail "Conditional with non-boolean"
      end
    | L.Select (label, t) => 
      let
        val tType = typeofenv (env, t)
      in
        (case tType of
          T.Record typePairs => (case List.find (fn (l, _) => l = label) typePairs of SOME (_, typ) => typ | NONE => raise Fail "Select of non-existent label")
        | _ => raise Fail "Select of non-record"
        )
      end

  fun typeof t = typeofenv (E.empty, t)

end
