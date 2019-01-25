functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y =
    let
    	(*给短的序列补0*)
        val s = tabulate (fn x => ZERO) (Int.max (length x,length y)-Int.min (length x,length y)+ 1)(*work O(n)  span O(1)*)
        val x1 = append (x,s)(*work O(n)  span O(log n)*)
        val y1 = append (y,s)
        val sum = zip x1 y1
        (*映射为carry序列*)
        fun find (ZERO,ZERO) = STOP
            |find (ONE,ONE) = GEN
            |find (_,_) = PROP   
        val sum1 = map find sum(*work O(n)  span O(log n)*)
        (*找进位*)
        fun copy (a,PROP) = a
           	|copy (_,GEN) = GEN
        	  |copy (_,STOP) = STOP
        val sum2 = scani copy STOP sum1
        val carry1 = append ((singleton STOP),sum2)(*和比原数多一位 w=O(n) s=1*)
        fun bit_add (ZERO,(ONE,ONE)) = ZERO
        	|bit_add (ZERO,(ZERO,ZERO)) = ZERO
        	|bit_add (ONE,(ONE,ZERO)) = ZERO
        	|bit_add (ONE,(ZERO,ONE)) = ZERO
        	|bit_add (_,(_,_)) = ONE
        (*进位1 无进位0*)
        val num1 = map (fn x => if x = STOP then ZERO else ONE ) carry1 (*work O(n)  span O(log n)*)
        val result1 = map2 bit_add num1 sum  (*work O(n)  span O(log n)*)
    in
    	result1
    end
    
  val add = op++
end