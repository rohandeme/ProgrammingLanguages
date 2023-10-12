structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun next [] = NONE
      | next (#"Z"::rest) = SOME(T.Z, rest)
      | next (#"T"::rest) = SOME(T.T, rest)
      | next (#"F"::rest) = SOME(T.F, rest)
      | next (#"S" :: rest) = SOME(T.S, rest)
      | next (#"P" :: rest) = SOME(T.P, rest)
      | next (#"+" :: rest) = SOME(T.Plus, rest)
      | next (#"-" :: rest) = SOME(T.Minus, rest)
      | next (#"[" :: rest) = SOME(T.LBrack, rest)
      | next (#"]" :: rest) = SOME(T.RBrack, rest)
      | next (#"<" :: rest) = SOME(T.LessThan, rest)
      | next (#">" :: rest) = SOME(T.GreaterThan, rest)
      | next (#"&" :: #"&" :: rest) = SOME(T.DoubleAmpersand, rest)
      | next (#"|" :: #"|" :: rest) = SOME(T.DoublePipe, rest)
      | next (#"?" :: rest) = SOME(T.QuestionMark, rest)
      | next (#":" :: rest) = SOME(T.Colon, rest)
      | next (c :: rest) = if Char.isSpace c then next rest else raise Fail "badchar"



  fun scan code = 
    let fun loop [] = []
      | loop chars = 
        (case next chars 
          of SOME (tok, chrs') => tok :: loop chrs'
            | NONE => []);
    in 
      loop (explode code)
    end
      
end
