functor MkBruteForcePD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  fun parenDist (parens : paren seq) : int option =
      raise NotYetImplemented
end
