functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq
  open Table

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  (* Define this type yourself *)
  type graph = weight table table

  fun makeGraph (E : edge Seq.seq) : graph = 
    let
      val edges = Seq.map (fn (v,u,w) => (v,(u,w))) E
      val edges_table = Table.collect edges
    in
      Table.map Table.fromSeq edges_table
    end


  fun findPath h G (S : Set.set, T :  Set.set) : (vertex * real) option = 
    let
      (*These codes are almost same as that given in Dijkstras.sml.*)
        fun N(v) =
          case Table.find G v
            of NONE => Table.empty ()
            | SOME nbr => nbr

      (*D: table that stores final results*)
      (*Q: tmp results in pq*)
        fun dijkstra' D Q =
          case PQ.deleteMin Q
            of (NONE, _) => D
             | (SOME (d, v), Q') =>
               case Table.find D v
                 of SOME _ => dijkstra' D Q'
                  | NONE =>
                    let
                      val insert = Table.insert (fn (v,_) => v)
                      val D' = insert (v, d) D
                      fun relax (q, (u, w)) = PQ.insert (d+w-h(v)+h(u), u) q
                      val Q'' = Table.iter relax Q' (N v)
                    in if not (Set.find T v) then dijkstra' D' Q'' else D'
                    end
        fun f_map x = Seq.map (fn s => (h(s),s)) x
        val S' = PQ.fromList(Seq.toList((f_map (Set.toSeq S))))
        val rst_table = dijkstra' (Table.empty ()) S'
        (*Only the vertices in T is needed.*)
        val rst = extract(rst_table, T)
        val ans = if (size rst) <> 0 then SOME (Seq.nth (toSeq rst) 0) else NONE
    in
      ans
    end

end
