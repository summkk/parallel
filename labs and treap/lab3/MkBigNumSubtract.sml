functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  exception NotYetImplemented
  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
      let
      	(*给短的序列y补0，xy符号位为0*)
        val y1 = if length x = length y then y else append (y,tabulate (fn x => ZERO) (length x-length y))
        val neg_y = map (fn x => if x = ZERO then ONE else ZERO) y1 (*取反*)
        val sub = x ++ (neg_y ++ singleton ONE)
        val result1 = take (sub,length x)
        (*找到第一个,高位1*)
        val cancel = zip (tabulate (fn i => i) (length x)) result1
        val find_one = filter (fn (x,y) => if y = ONE then true else false) cancel
        val (a,b) = nth find_one (length find_one-1)
      in
      	take (result1,a + 1)
      end
      
  val sub = op--
end
(*fun copy (ZERO, ZERO) = ZERO
           |copy (ZERO, ONE) = ONE
           |copy (ONE, _) = ONE
        val find_zero = scani copy ZERO (rev result1)逆序
        val num_zero = length (filter (fn x => if x = ZERO then true else false) find_zero) *)