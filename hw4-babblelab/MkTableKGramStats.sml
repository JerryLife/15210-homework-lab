functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq

  type token = string
  type kgram = token seq

  (* You must define the abstract kgramstats type *)
  type kgramstats = ((token hist) Table.table) * int
  (*A table of several token hists is the main part
    of kgramstats. The final int is used for maxK*)

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
      let
        (*Get all the words in a sequence ignoring '.' and ' ' which are not alpha or number*)
        val words = tokens (not o Char.isAlphaNum) corpus

        (*Form the basic of the table*)
        fun make_table words' maxK' =
          if maxK' = 0 then 
            map (fn s => (empty (), s)) words'
          else
            let
              (*Bound each word with maxK' words following. And add them to a sequence*)
              fun make_keys i = subseq words' (i,maxK')
              val keys = tabulate make_keys (length words' - maxK')
              val after = drop (words', maxK')
              val rst = append (make_table words' (maxK'-1), zip keys after)
            in
              rst
            end

        (*Use the data above to form the kgramstats*)
        val table_data = make_table words maxK
        val table_demo = collect (collate String.compare) table_data
        val counts = map (fn s => histogram String.compare (#2 s)) table_demo
        val keys = map (fn s => (#1 s)) table_demo
        val ks = (Table.fromSeq (zip keys counts), maxK)
      in
        ks
      end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq =
      getOpt (Table.find (#1 stats) kgram, empty ())

  fun maxK (stats : kgramstats) : int =
      #2 stats
end
