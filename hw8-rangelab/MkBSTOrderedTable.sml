functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table
  open Tree

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)
  exception NYI

  fun first (T : 'a table) : (key * 'a) option =
    case expose T 
      of NONE => NONE
       | SOME {key, value, left, right} =>
          if size left = 0 then SOME (key, value) else first left

  fun last (T : 'a table) : (key * 'a) option =
    case expose T 
      of NONE => NONE
       | SOME {key, value, left, right} =>
          if size right = 0 then SOME (key, value) else last right

  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    last (#1 (splitAt (T,k)))

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    first (#3 (splitAt (T,k)))

  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join (L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt (T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let
      val (L, f1, R) = split (T, low)
      val (L', f2, R') = split (R, high)
      val (l, r) = (if isSome(f1) then singleton (low, valOf f1) else empty(),
                    if isSome(f2) then singleton (high, valOf f2) else empty())
    in
      join (join (l,L') ,r)
    end

end
