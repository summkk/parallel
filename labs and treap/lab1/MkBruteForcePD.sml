functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun matchParens parens =
       let
         val s = map (fn x=>if x = OPAREN then 1 else ~1) parens
         val (s1,ans) = scan (op +) 0 s
         fun copy(a,b) = if a <b then a else b
         val (s2,ans2) = scan copy 0 s1
       in
         if ans <> 0 orelse ans2 < 0 then false else true
       end

  fun dis (parens,i,j) = (*左右括号最大匹配的距离*)
      if nth parens j = CPAREN andalso nth parens i = OPAREN andalso matchParens (subseq parens (i+1,j-i-1))
      then SOME(j-i-1)
      else NONE
  
  fun cmp (NONE,NONE) = NONE
      |cmp(SOME a,NONE) = SOME a
      |cmp(NONE,SOME a) = SOME a
      |cmp(SOME a,SOME b) = if (a>b) then SOME a else SOME b 

  fun parenDist (parens : paren seq) : int option =(*o(n4)*)
    let
      val max = 0
      fun loop (i,j) =
        if i = length parens then NONE
        else if j = length parens 
             then loop (i+1,i+1)
             else cmp(dis (parens,i,j), loop (i,j+1))
    in
      if matchParens parens = false then NONE
      else loop (0,0)
    end

end
 (*fun matchParens parens =
       let
         fun count(NONE,_) = NONE
            |count(SOME 0,CPAREN) = NONE
            |count(SOME n,CPAREN) = SOME(n-1)
            |count(SOME n,OPAREN) = SOME(n+1)
       in
         (iter count(SOME 0) parens) = SOME 0
       end*)