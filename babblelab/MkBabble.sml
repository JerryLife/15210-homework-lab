functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq

  exception NoData

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
      let
        val ran_seq = R.randomRealSeq seed NONE n
        val maxK = KS.maxK(stats)
        fun make_st st i = 
          if i = length ran_seq then empty()
          else let
            val kgram = if i <= maxK then st else drop (st, i-maxK)
            val next_hist = KS.lookupExts stats kgram
            val next_word = Util.choose next_hist (nth ran_seq i)
            val new_st = append(st, singleton next_word)
          in
            append(singleton next_word, make_st new_st (i+1))
          end
        val ran_st = make_st (empty()) 0
      in
        (String.concatWith " " (toList ran_st)) ^ "."
      end


  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
      let
        val st_len_seq = R.randomIntSeq seed (SOME(5, 10)) n
        val seeds_seq = map R.fromInt (R.randomIntSeq seed NONE n)
        val monkey_doc = tabulate 
          (fn i => randomSentence stats (nth st_len_seq i) (nth seeds_seq i)) n
      in
        String.concatWith " " (toList(monkey_doc))
      end

end
