functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
    let
      fun f_sum ((c1, v1), (c2, v2)) = (c2, v1 + v2)
      val freq = scani f_sum (#1 (nth hist 0), 0) hist
      val all_freq = #2 (nth freq (length freq - 1))
      fun map_freq (c, p) = (c, Real.fromInt p / Real.fromInt all_freq)
      val cum_dist = map map_freq freq
      fun search cum_dist'=
        case showt cum_dist' of
        ELT (c: 'a, cd: real) => c
        | NODE ((l: ('a * real) seq), (r: ('a * real) seq)) =>
          if #2 (nth l (length l - 1)) < p then 
            search r 
          else search l
    in
      search cum_dist
    end
end
