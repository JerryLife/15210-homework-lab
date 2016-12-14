structure Tests =
struct
  val testsAdd : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937),
    (1,1),
    (0,1),
    (1,0),
    (1250,3365),
    (3365,1250),
    (223,445),
    (3,5),
    (12,25),
    (25,12),
    (26,56),
    (72,121),
    (0,0)
  ]

  val testsSub : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (1024, 937),
    (12516,5266),
    (1,0),
    (454,32),
    (353,2)
  ]

  val testsMul : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937)
  ]

end
