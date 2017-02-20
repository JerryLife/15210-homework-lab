functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  (* Uncomment this line once you're done *)
  exception NotYetImplemented

  (* Define this type yourself *)
  type graph = unit

  fun makeGraph (E : edge Seq.seq) : graph = raise NotYetImplemented

  fun findPath h G (S, T) = raise NotYetImplemented

end
