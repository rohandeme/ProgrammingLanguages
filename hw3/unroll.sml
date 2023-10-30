structure Unroll : sig

  val unroll : Sweetl.prog -> Sweetl.term

end = struct
  structure S = Sweetl

    fun unroll (S.Prog (abbvs, (S.App (t1, t2)))) = (S.App (unroll (S.Prog (abbvs, t1)), unroll (S.Prog (abbvs, t2))))
      | unroll (S.Prog (abbvs, (S.Lam (x, t)))) = (S.Lam (x, unroll (S.Prog (abbvs, t))))
      | unroll (S.Prog (abbvs, (S.Abbr s))) =  
        let val mapping = List.find (fn (abbr, term) => abbr = s) abbvs
        in
          case mapping of 
            SOME (abbr, term) => unroll (S.Prog (abbvs, term))
            | NONE => raise Fail "Abreviation not in list"
        end
      | unroll (S.Prog (abbvs, x)) = x
				 
end
