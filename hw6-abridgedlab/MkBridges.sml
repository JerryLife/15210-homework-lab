functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  (* Remove these two lines before submittting. *)
  exception NotYetImplemented

  type ugraph = unit

  fun makeGraph (E : edge seq) : ugraph = raise NotYetImplemented

  fun findBridges (G : ugraph) : edges = raise NotYetImplemented

end
