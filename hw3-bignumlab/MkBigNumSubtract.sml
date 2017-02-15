functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --

  fun x ++ y = BNA.add (x, y)
  fun x -- y =
      let
        (*Make x & y the same length*)
        val len = length x - length y
        val (x_full, y_full) = 
          if len > 0 then (x, append (y, tabulate (fn _ => ZERO) len))
          else(*len <= 0*)(append (x, tabulate (fn _ => ZERO) (~len)), y)
        fun change x = if x = ONE then ZERO else ONE
        val y_rev = map change y_full
        val y_cpm = y_rev ++ (singleton ONE)
        val rst = x_full ++ y_cpm
      in
        if length rst = length x then rst
        else take (rst, (length rst) - 1)
      end

  val sub = op--
end
