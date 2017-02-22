functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  fun findSegments (E, n) initial_credit =
    let
      val RANDSEED = 4843
      val vertices = tabulate (fn i => i) n
      (*Sort the edges by weight (from Large to Small)*)
      val edge_sort = sort (fn ((v1,u1,w1),(v2,u2,w2)) => Int.compare (w2,w1)) E
      val edges = map (fn (v,u,w) => (v,(u,w))) edge_sort
      val credits = tabulate (fn _=> initial_credit) n
      fun get_map s = map (fn v => nth s v) s
      fun fs (V : vertex seq, E : (vertex * (vertex * int)) seq, C : int seq) (seed : Rand.rand) =
        let
          val edges_min_data = inject E (tabulate (fn _=> (~1,0)) n)
          val edges_min = filter (fn (_,(v,_)) => v <> ~1) (enum edges_min_data)
          val V_flag_rand = Rand.flip seed n
          fun get_r r = nth V_flag_rand r
          (*filter the randomly chosen minimun edges to contract*)
          val edges_chosen = filter (fn (v,(u,_)) =>
            if get_r v = 0 andalso get_r u = 1 then true else false) edges_min
          (*delete the contracting vertices*)
          val v_contract = map (fn (v,(u,_)) => (v,u)) edges_chosen

          (*use credit to contract*)
          fun rev_v (v,(u,w)) = (u,(v,w))
          val v_zip = Seq.collect Int.compare (map rev_v edges_chosen)
            (*get the sum of weight of neighbors for each destination vertex*)
          val zip = reduce op+ 0 (map (fn x => #2 x) zips))
          val sum_ws = map (fn (u,zips) => (u, zip) v_zip
            (*function to get the minimum creditt of a single vertex*)
          fun get_credits (v,zips) = append (%[nth C v], map (fn (u,_) => nth C u) zips)
            (*the minimum credit of v and v's neighbors*)
          val min_credit = map (fn (v,zips) => (v, reduce Int.min initial_credit (get_credits (v,zips)))) v_zip
            (*calculate the new credits*)
          val C' = inject min_credit C
          val new_C = inject (map (fn (v,w) => (v,nth C' v - w)) sum_ws) C'

          (*Renew the parameters*)
          val new_V = get_map (inject v_contract V)
          fun get_root v = nth new_V v
          fun del_rep s = filter (fn (v,(u,_)) => v <> u) s
          fun del_err s = filter (fn (v,(u,w)) => w < Int.min(nth new_C v, nth new_C u)) s
          val new_E = del_err (del_rep (map (fn (v,(u,w)) => (get_root v, (get_root u, w))) E))
        in        
          if length E = 0 then V
          else fs (new_V, new_E, new_C) (Rand.next seed)
        end
    in
      get_map (fs (vertices, edges, credits) (Rand.fromInt RANDSEED))
    end

end
