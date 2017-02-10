functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = ((vertex seq) table) * int * int
  (*Each value of the table contains all edges
   adjacent to the key vertex, the final 2 int
   refers to the number of vertice and the num-
   ber of edges*)
  type asp = vertex seq table
  (*Each vertex (key) maps to its parents, all
  v's parents are in the shhortest paths from
  u (start point) to v, like:
    or   ASP                   Graph
    |7 -> <5,6>               1——4——6
    |5 -> <2,3>              / \    |
    |2 -> <1>               2   3   |
    |3 -> <1>                \ /    |
    |6 -> <4>                 5—————7
    |4 -> <1>
  which stands because 'The subpath of a shortest
  path is a shortest path' *)

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    let
      val table_graph = Table.collect E
      val vertice = flatten (map (fn (a,b) => % [(a,0), (b,0)]) E)
      val v_num = size (Table.collect vertice)
      val e_num = length E
    in
      (table_graph, v_num, e_num)
    end

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    #3 G

  fun numVertices (G : graph) : int =
    #2 G

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    getOpt(Table.find (#1 G) v, empty())

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      (*v_asp : the result asp; p : parents vertices;
       visited : all vertices that has been visited*)
      fun BFS (v_asp : asp) (p : vertex seq) (visited : Set.set) : asp =
        if length p = 0 then v_asp
        else let
          (*Ng : list the out-neighbors that haven't been visited*)
          fun Ng v = filter (fn u => not (Set.find visited u)) (outNeighbors G v)
          val nexts = map (fn v => (v, Ng v)) p
          (*nexts : <(a, <b,c,d>), (e, <d,g>),...>*)
          (*get_new : Generate all new vertices in asq*)
          fun get_new (next : vertex * vertex seq) : (vertex * vertex) seq = 
            map (fn s => (s, #1 next)) (#2 next)
          val asp_add : asp = Table.collect (flatten (map get_new nexts))
          val new_p = Seq.flatten (map (fn (a,b) => b) nexts)
          val new_asp = Table.merge Seq.append (v_asp, asp_add)
          val new_visited = Set.union (visited, Set.fromSeq new_p)
        in
          BFS new_asp new_p new_visited
        end
      val init_v = Table.singleton (v, singleton v)
    in
      BFS init_v (Seq.singleton v) (Set.empty())
    end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    let
      fun find_parent x = getOpt(Table.find A x, Seq.empty ())
      (*v_asp : compare 2 vertices*)
      fun v_cmp (a : vertex, b : vertex) = 
        if Table.Key.equal (a, b) then EQUAL else LESS
      (*del_rep : delete repeated paths*)
      fun del_rep (s : vertex seq seq) = 
        map (fn p => #1 p) (Seq.collect (collate v_cmp) 
          (zip s (tabulate (fn _ => ()) (length s))))
      fun make_path v : vertex seq seq =
        if length (find_parent v) = 0 then empty()
        else if v_cmp (nth (find_parent v) 0, v) = EQUAL then %[%[v]]
        else let
          val nexts_data = find_parent v
          val nexts : vertex seq seq seq = map make_path nexts_data
        in
          flatten (map (fn x => map (fn y => append (y, %[v])) x) nexts)
        end
    in
      del_rep(make_path v)
    end
end