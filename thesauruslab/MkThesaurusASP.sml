functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = ASP.graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    ASP.makeGraph (flatten (map (fn (a,b) => map (fn s => (a,s)) b) S))

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    ASP.outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    let
      val dict = ASP.makeASP T w1
    in
      ASP.report dict w2
    end

end
