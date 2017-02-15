functor MkBruteForcePD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
  	let
      fun parenMatch p =
		let
		fun pm ((NONE, _) | (SOME 0, CPAREN)) = NONE
			| pm (SOME c, CPAREN) = SOME (c - 1)
			| pm (SOME c, OPAREN) = SOME (c + 1)
		in
			iter pm (SOME 0) p = (SOME 0)
		end
	  val max = NONE
	  fun check1 i = (*First Loop*)
	  	if i = length parens then NONE
	  	else
	  	  let
	  		fun check2 j = (*Second Loop*)
	  			if j = length parens then NONE
	  			else
	  			  let
	  			  	val s = subseq parens (i, j-i+1)
	  			  in
	  				if parenMatch s then Option210.intMax(SOME (length s - 2), max)
	  				else check2 (j+1)
	  			  end
	  	  in
	  		Option210.intMax(check1 (i+1), check2 (i+1))
	  	  end
  	in
  		if not (parenMatch parens) then NONE
  		else check1 0
  	end
end