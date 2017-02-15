functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph = 
    let
      val directed_edges = append (E, map (fn (a,b) => (b,a)) E)
      val guide = collect Int.compare directed_edges
    in
      map (fn (a,b) => b) guide
    end

  fun findBridges (G : ugraph) : edges = 
    let
      val START = ~1
      val INF =  (length G) * (length G)
      fun DFS (p : vertex) ((X : int option seq, min : int, DFS_i : int, Rst : edges), v : vertex) =
        case nth X v of
          SOME low => (X, Int.min (low, min), DFS_i, Rst)
          |NONE =>
            let
              val Ng_unvisited = filter (fn u => u <> p) (nth G v)
              val new_X = inject (%[(v, SOME DFS_i)]) X
              val (X', min', DFS_i', Rst') = iter (DFS v) (new_X, INF, DFS_i+1, Rst) Ng_unvisited
              val min'' = Int.min (min', DFS_i)
              val new_bridge : edges = 
                if p <> START andalso min'' = DFS_i then singleton (p,v) else empty()
              val new_Rst = append (new_bridge, Rst')
              val new_min = Int.min (min'', min)
            in
              (new_X, new_min, DFS_i+1, new_Rst)
            end
      val init_X = tabulate (fn _=> NONE) (length G)
      val init = (init_X, INF, 0, empty())
      val rst = iter (DFS START) init (tabulate (fn i => i) (length G))
      fun pair_cmp ((a:int,b:int),(c:int,d:int)) = if a=c then Int.compare(b,d) else Int.compare(a,c)
      fun del_rep s = map (fn s => #1 s) (Seq.collect pair_cmp (zip s (tabulate (fn _=> 1) (length s))))
    in
      del_rep (#4 rst)
    end

end
