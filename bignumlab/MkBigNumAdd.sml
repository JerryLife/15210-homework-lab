functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y =
      let
        fun combine ((ONE,ZERO) | (ZERO,ONE)) = PROP
          |combine (ONE,ONE) = GEN
          |combine (ZERO,ZERO) = STOP
        val len = length x - length y
        val xy = 
          if len > 0 then map2 combine x (append (y, tabulate (fn _ => ZERO) len))
          else if len = 0 then map2 combine x y
          else map2 combine y (append (x, tabulate (fn _ => ZERO) (~len)))
        fun copy (c1,c2) =
          if c2 = PROP then c1 else c2
        val (rst,last) = scan copy STOP xy   (*rst contains only GEN and STOP*)
        fun calc ((STOP|GEN),c) = if c = GEN then ONE else ZERO
          |calc (PROP,c) = if c = GEN then ZERO else ONE
        val rst' = map2 calc xy rst        
      in
        if last = GEN then append (rst',singleton ONE)
        else rst'
      end

  val add = op++
end