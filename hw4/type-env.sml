structure TypeEnv :> sig

  type env = (string * Type.typ) list

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
	    
end = struct

  type env = (string * Type.typ) list (* todo: replace this *)
	       
  val empty = [] (* todo: replace this *)

  fun lookup (env, str) = (case List.find (fn (name, _) => name = str) env of SOME (_, typ) => SOME typ | NONE => NONE)
  fun extend (env, str, typ) = (str, typ) :: env
					  
end
