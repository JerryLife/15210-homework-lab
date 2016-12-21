functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
    if (length x = 1 andalso nth x 0 = ZERO) orelse (length y = 1 andalso nth y 0 = ZERO)
      then singleton ZERO
    else if (length x = 1 andalso nth x 0 = ONE) andalso (length y = 1 andalso nth y 0 = ONE)
      then singleton ONE
    else
      let
        (**to make x,y the same length *)
        val len = length x - length y
        val (x_full, y_full) = 
          if len > 0 then (x, append (y, tabulate (fn _ => ZERO) len))
          else (append (x, tabulate (fn _ => ZERO) (~len)), y)
        (*q & s indicates the lower place, and p & r the higher*)
        val (q,p) = case showt x_full of
          EMPTY => (singleton ZERO, singleton ZERO)
          | ELT x0 => (singleton x0, singleton ZERO)
          | NODE (q',p') => (q',p')
        val (s,r) = case showt y_full of
          EMPTY => (singleton ZERO, singleton ZERO)
          | ELT x0 => (singleton x0, singleton ZERO)
          | NODE (s',r') => (s',r')
        val move = length s (*concentrate on the function 'showt'!*)
        val (high_part, mid_part,low_part) = par3(
          fn _ => (append ((tabulate (fn _ => ZERO) (move * 2)), (p ** r))),
          fn _ => (append ((tabulate (fn _ => ZERO) move), ((p ** s) ++ (r ** q)))),
          fn _ => (q ** s))
      in
        high_part ++ mid_part ++ low_part
      end
      
  val mul = op**
end