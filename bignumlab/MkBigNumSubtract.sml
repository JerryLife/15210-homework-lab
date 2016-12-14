functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --

  fun x ++ y = BNA.add (x, y)
  fun x -- y =
      let
        val len = length x - length y
        val (x_full, y_full) = 
          if len > 0 then (x, append (y, tabulate (fn _ => ZERO) len))
          else (append (x, tabulate (fn _ => ZERO) (~len)), y)
        fun change x = if x = ONE then ZERO else ONE
        val y' = map change y_full
        val y'' = y' ++ (singleton ONE)
        val rst = x_full ++ y''
      in
        if length rst = length x then rst
        else take (rst, (length rst) - 1)
      end

  val sub = op--
end
