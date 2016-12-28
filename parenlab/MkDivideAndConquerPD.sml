functor MkDivideAndConquerPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
(*Struct :  )))))())))Match(((((()((((  *)
(*********************************************************************
 'l'     : the number of unmatched CPARENs
 'llen'  : the largest distance from the unmatched CPAREN to the left
 'match' : the number of matched PARENs
 'rlen'  : the largest distance from the unmatched OPAREN to the right
 'r'     : the number of unmatched OPARENs
**********************************************************************)
  fun parenDist (parens : paren seq) : int option =
      let
      	fun pd s =
      		case showt s of
      			EMPTY        => (0,0,0,0,0)
      			| ELT OPAREN => (0,0,0,1,1)
      			| ELT CPAREN => (1,1,0,0,0)
      			| NODE (l,r) =>
      			let
      				val ((l1, llen1, match1, rlen1, r1), (l2, llen2, match2, rlen2, r2)) = 
                  par (fn _ => pd l, fn _ => pd r)
      			in
      				if r1 = l2 then
      					let
      						val max_match = Int.max (rlen1+llen2, Int.max (match1, match2))
      					in
      						(l1, llen1, max_match, rlen2, r2)
      					end
      				else if r1 > l2 then
      					let
      						val max_rlen = rlen1 + llen2 + match2 + rlen2
      					in
      						(l1, llen1, match1, max_rlen, r1-l2+r2)
      					end
      				else (*if r1 < l2 then*)
      					let
      						val max_llen = llen1 + match1 + rlen1 + llen2	
      					in
      						(l1+l2-r1, max_llen, match2, rlen2, r2)
      					end
      			end      			
      	val (l0, llen0, match0, rlen0, r0) = pd parens
      in
      	if l0 = 0 andalso r0 = 0 then    (*Deal with empty*)
      		SOME (match0 - 2)
      	else NONE
      end
end
