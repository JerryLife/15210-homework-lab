functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (* Remove the following two lines when you're done! *)
  exception NYI
  type nyi = unit

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = nyi

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    raise NYI

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    raise NYI

  fun synonyms (T : thesaurus) (w : string) : string seq =
    raise NYI

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    raise NYI

end
