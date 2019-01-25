functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  fun combine s1 s2= (*w=n s=log n *)
  	   let
		    fun cmp ((l1,r1),(l2,r2)) = if l1 < l2 then LESS else if l1 = l2 then EQUAL else GREATER
		    
		    (*s3 s4 仅保存需要的位置*)
		    val s3 = map (fn (x1,y1) => (x1,~1)) s1 (*work O(n)  span O(log n)*)
		    val s4 = map (fn (x2,y2) => (x2,~1)) s2
		    (*左右分别合并位置信息然后copy高度*)
		    fun copy ((x3,y3),(x4,y4)) = if y4 = ~1 then (x4,y3) else (x4,y4)
		    (*merge w=O(n) s=O(log n)  scan w=O(n) s=O(logn)*)
		    val building1 = scani copy (0,~1) (merge cmp s1 s4) 
		    val building2 = scani copy (0,~1) (merge cmp s2 s3)
		    (*选择更高的，并且一样高的去重*)
            fun maxheight ((x1,y1),(x2,y2)) =  if y1 > y2 then (x1,y1) else (x2,y2)
            (*w=o(n) s=o(1)*)    
		    val rem1 = map2 maxheight building1 building2
		    fun judge (0,_) = true
		    	|judge (i,a:int*int) = if (#2 a) <>(#2 (nth rem1 (i-1))) then true else false
		in(*去重*)
			(*w=o(n) s=log n*)
		    filterIdx judge rem1
		end
 
  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
  	(*w(n) = 2w(n/2) + o(n)  s(n) = s(n/2) + o(log n)   w = o(nlogn) s = o(log2n)*)
		if length buildings = 0 then empty()
		else
		case showt buildings of(*w= lgn or n*)
		    EMPTY => singleton((0,0))
		    |ELT (l,h,r) => fromList [(l,h),(r,0)]
		    |NODE (s1,s2) => combine (skyline s1) (skyline s2)   		    
end