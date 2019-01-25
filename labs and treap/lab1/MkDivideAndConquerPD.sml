functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

(*  左边的剩余,左边距离,右边的剩余,右边距离,最大值
  	如果左边的（和右边的）相等 匹配
WparenDist(n) = 2 · WparenDist 2n + Wshowt(n) + O(1)
SparenDist(n) = SparenDist 2n + Sshowt(n) + O(1)*)
  fun cmp s1 s2 = if s1 = s2 then EQUAL
  	              else if s1 > s2 then GREATER 
  	              else LESS
  fun parenDist (parens : paren seq) : int option = 
       let
       	    fun dis par = case showt par of
  	             EMPTY => (0,0,0,0,SOME 0) 
               	|ELT OPAREN => (1,0,0,0,SOME 0)
  	            |ELT CPAREN => (0,0,1,0,SOME 0)
  	            |NODE (parens1,parens2) =>
  	                let 
  	      	           val ((l1,ldis1,r1,rdis1,max1),(l2,ldis2,r2,rdis2,max2))= Primitives.par(fn()=>dis parens1,fn()=> dis parens2)
  	      	           val (l3,ldis3,r3,rdis3,max3) = 
  	      	               case cmp l1 r2 of
  	      	           	       EQUAL =>(l2,ldis2,r1,rdis1,SOME(ldis1 + rdis2))
  	      	           	      |GREATER =>(l1 + l2 - r2,ldis1 + length parens2,r1,rdis1,Option210.intMax(max1,max2))
  	      	           	      |LESS => (l2,ldis1,r1 + r2 - l1,rdis2 + length parens1,Option210.intMax(max1,max2))

   	                in  	                   
                      (l3,ldis3,r3,rdis3,max3)
                    end
                   
            val (l,_,r,_,maxf) = dis parens
       in
       	    if length parens > 0 andalso l = 0 andalso r = 0 then maxf
       	    else NONE
       end

end
