functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = unit table table

  fun makeCountTable (S : point seq) : countTable =
    if Seq.length S = 0 then empty()
    else let
      val K = #1 (Seq.nth S 0)
      fun cmp ((x1, y1), (x2, y2))= Key.compare (x1, x2)
      val sort_x = Seq.sort cmp S
      val sort_x' = Seq.map (fn (a,b) => (a, (b, ()))) sort_x
      fun insert (s, (x, ())) : unit table = 
        let
          val (L,_,R) = split (s, x)
        in
          join (join (L, singleton (x, ())), R) 
        end 
      fun iter_x ((a,s),(b,x)) = (b, insert (s, x))
      val (table_data, rst) = Seq.iterh iter_x (K ,empty()) sort_x'
    in
      fromSeq (Seq.append(Seq.drop (table_data, 1), Seq.singleton rst))
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    if size T = 0 then 0 
    else let
      val x_left = getRange (if isSome(previous T xLeft) then #2 (valOf(previous T xLeft)) else empty()) (yLo, yHi)
      val x_right = getRange (getOpt(#2 (split (T, xRght)), (#2 (valOf(previous T xRght))))) (yLo, yHi)
    in
      (size x_right) - (size x_left)
    end
end
