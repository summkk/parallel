functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)

  fun bit_mul (ONE,ONE) = ONE
           |bit_mul (_,_) = ZERO
  (*乘法*)
  fun multiply sum =
      case showt sum of
          EMPTY => empty()
          |ELT (a,b) => singleton (bit_mul (a,b))
          |NODE (x,y) =>
           let 
              val p = map (fn (a,b) => a) y
              val q = map (fn (a,b) => a) x
              val r = map (fn (a,b) => b) y
              val s = map (fn (a,b) => b) x
              val t1 = p++q
              val t2 = r++s
              val t3 = tabulate (fn x => ZERO) (Int.max (length t1,length t2)-Int.min (length t1,length t2))
              val t = zip (append (t1,t3)) (append (t2,t3))
              val (pr,pqrs,qs) = par3 (fn () => multiply y,fn () => multiply t,fn () => multiply x)
              fun power (m,(n:int)) = append ((tabulate (fn i => ZERO) n),m)(*给m前补n位0 变高位*)
              val k = length x
           in
              qs ++ (power (pr,2*k)) ++ (power ((pqrs--pr--qs),k))
           end

  fun x ** y = 
    let
      (*给短的序列补0*)
        val s = tabulate (fn x => ZERO) (Int.max (length x,length y)-Int.min (length x,length y))
        val x1 = append (x,s)
        val y1 = append (y,s)
        val sum = zip x1 y1
    in
        if length x = 0 orelse length y = 0 then empty()
        else if length x = 0 then y
        else if length y = 0 then x 
        else  multiply sum        
    end
         
  val mul = op**
end
(*A · B = pr · 2n + (ps + rq) · 2n=2 + qs  hint:
(p + q) ∗ (r + s) = pr + ps + rq + qs
ps + rq = (p + q) ∗ (r + s) − pr − qs
补零实现高位*)