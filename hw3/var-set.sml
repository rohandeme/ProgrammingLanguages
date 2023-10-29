structure VarSet :> sig

  type set = string list

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set

end = struct

  type set = string list (* <== Change this to something else! *)

  val empty = [] (* <== Change this to something consistent with the new set type. *)

  fun mem (x, set) = List.exists (fn s => s = x) set

  fun ins (x, set) = if mem (x, set) then set else x::set

  fun rem (x, set) = List.filter (fn s => s <> x) set

  fun union (set1, set2) = List.foldl (fn (x, s) => ins(x, s)) set1 set2
				      
end
