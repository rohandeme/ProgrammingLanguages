structure Test = struct

  structure L = L23RR

  fun println s = (print s; print "\n")

  fun testing x = println ("testing " ^ x ^ "...")

  fun parseType (concreteType : string) : Type.typ =
   (case Parse.parse (Scan.scan ("[lam x " ^ concreteType ^ " ()]"))
      of L.Lam (_, tau, _) => tau
       | _ => raise Fail "bug in parseType; this should never happen")

  fun subty () =
    let
      fun chkT t1 t2 = Check.assertT (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (true)")
      fun chkF t1 t2 = Check.assertF (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (false)")
      val _ = testing "subty"
      val _ = chkT "(~a I ~b B)" "(~a I)"
      val _ = chkF "(~a I)" "(~a I ~b B)"
      val _ = chkT "(~a I ~b B)" "(~a I ~b B)"
    in
      println "subty tests done"
    end

  fun all () =
    let
      val _ = subty ()
    in
      println "all tests done"
    end

end
