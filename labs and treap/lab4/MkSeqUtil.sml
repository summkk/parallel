functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  exception NotYetImplemented
  exception Range

  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
    if length hist = 0 then raise Range
    else if p > 1.0 orelse p < 0.0 then raise Range
    else 
       let
         val s1 = map #2 hist
         val s2 = scani op+ 0 s1(*得到概率密度*)
         val a = nth s2 (length s2 - 1 )(*次数和*)
         val s3 = map (fn i => real(i)) s2
         val s4 = zip (tabulate (fn i=>i) (length s3) ) s3 
         val b = p*(Real.fromInt a)
         fun fil (0,x) = if b <= x then true else false
            |fil (i,x) = if b <= x andalso b >(nth s3 (i-1)) then true else false
         val s5 = filter fil s4
       in
         #1 (nth hist (#1 (nth s5 0)))
       end

end
