structure Tests =
struct

  (* Do not remove the following line! *)
  val corpus = TextIO.inputAll (TextIO.openIn "corpus.txt")

  val testsChoose : (((string * int) list) * real) list  = [
    (*Empty sequence*)
    ([], 0.2),
    (*Out of right range*)
    ([("test", 2), ("awesome", 2)], 1.5),
    (*Out of left range*)
    ([("test", 2), ("awesome", 2)], ~0.1),
    (*One string only*)
    ([("test", 10)], 0.5),
    (*Exactly equal*)
    ([("test", 2), ("awesome", 2)], 0.5),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.47),
    (*Try something longer*)
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4), ("hello", 3), ("bye", 4)], 0.56)
  ]

  (* You must add test kgrams for the corpus in corpus.txt as part of task 5.5
   * You may edit corpus.txt -- it will be handed in.
   *
   * You may also add other tests, which use other corpi (corpuses?), but those
   * corpuses will not be submitted. *)
  val testsKGramStats : ((string * int) * (string list)) list = [
    ((corpus, 5),
        [(*Try some single words*)
        "direction",
         "time",
         (*Try some multiple words*)
         "direction of time",
         "would write",
         "What Eddington says about",
         (*See if special punctuation mark matters*)
         "to this",
         (*The empty should map to a hist made of all words*)
         "",
         (*Try a word not existing in the corpus*)
         "Encryption"
         ])
  ]


end
