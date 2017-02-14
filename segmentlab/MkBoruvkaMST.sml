functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  fun MST (E : edge seq, n : int) : edge seq =
    let
      (*Sort the edges by weight (from Large to Small)*)
      val edge_sort = sort (fn ((v1,u1,w1),(v2,u2,w2)) => Int.compare (w2,w1)) E
      val vertices = tabulate (fn i => i) n
      val edges = map (fn (v,u,w) => (v,(u,(v,u,w)))) edge_sort
      fun mst (V : vertex seq, E : (vertex * (vertex * edge)) seq, T : edge seq) (seed : Rand.rand) =
        if length E = 0 then T
        else let
          val edges_min_data = inject E (tabulate (fn _=> (~1,(0,0,0))) n)
          val edges_min = filter (fn (_,(v,_)) => v > ~1) (enum edges_min_data)
          val V_flag_rand = Rand.flip seed n
          fun get_r r = nth V_flag_rand r
          val edges_contract = filter (fn (v,(u,_)) =>
            if get_r v = 0 andalso get_r u = 1 then true else false) edges_min
          val v_contract = map (fn (v,(u,_)) => (v,u)) edges_contract
          (*delete the contracting vertices*)
          val new_V = inject v_contract V
          val new_T = append (map (fn (v,(u,e)) => e) edges_contract, T)
          fun get_root v = nth new_V v
          fun del_rep s = filter (fn (v,(u,_)) => v <> u) s
          val new_E = del_rep (map (fn (v,(u,e)) => (get_root v, (get_root u, e))) E)
        in
          mst (new_V, new_E, new_T) (Rand.next seed)
        end
    in
      mst (vertices, edges, empty()) (Rand.fromInt 4843)
    end

end
