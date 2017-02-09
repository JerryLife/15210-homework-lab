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
  type asp = (vertex seq seq) table
  (*Each vertex sequence is a shortest path, and
  each parent vretex has several shortest paths
  like
    {(a,<<b,c,d>>),(e,<<f,g,h>,<f,c,h>>),...}
    or
    |v1 -> <<a,b,c,d,...,v1>
    |       <a,g,c,d,...,v1>>
    |---------------------------
    |v2 -> <<a,g,h,...,v2>>
    |---------------------------
    |v3 -> ...
    in which all the paths start at original vertex
    'a' and end at the key vertex vn*)

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
      fun BFS (v_asp : asp) (p : vertex seq) (visited : Set.set) : asp =
        if length p = 0 then v_asp
        else let
          fun Ng v = filter (fn u => not (Set.find visited u)) (outNeighbors G v)
          val nexts = map (fn v => (v, Ng v)) p
          (*next : <(a, <b,c,d>), (e, <f,g>),...>*)
          (*get_new : Generate all new sequences in asq from a parent*)
          fun get_new i (next : vertex * vertex seq) : (vertex * vertex seq) seq = 
            if i >= length (#2 next) then empty()
            else let
              val parents : vertex seq seq = 
                getOpt(Table.find v_asp (#1 next), empty())
              val one = nth (#2 next) i
              val add_one = map (fn u => 
                (one, Seq.append(u, singleton one))) parents 
            in
              Seq.append (add_one, get_new (i+1) next)
            end
          (*asp_add : Map get_new to all parents*)
          val asp_add = Table.collect (flatten (map (get_new 0) nexts))
          val new_p = Seq.flatten (map Ng p)
          val new_asp = Table.merge (fn (a,b) => b) (v_asp, asp_add)
          val new_visited = Set.union (visited, Set.fromSeq new_p)
        in
          BFS new_asp new_p new_visited
        end
      val init_v = Table.singleton (v, singleton (singleton v))
    in
      BFS init_v (Seq.singleton v) (Set.empty())
    end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    getOpt(Table.find A v, Seq.empty ())
end
