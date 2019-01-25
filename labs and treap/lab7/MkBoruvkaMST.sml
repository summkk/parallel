functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Remove this exception when you're done! *)
  exception NotYetImplemented

  fun MST (E : edge seq, n : int) : edge seq =
      let
        fun cmp ((u1,v1,w1),(u2,v2,w2)) = Int.compare (w1,w2)
        val sorte = rev (sort cmp E)(*转换为从大到小*)
        val edge = map (fn (u,v,w) => (u,(v,(u,v,w)))) sorte (*边作为lable*)
        val vertices = tabulate (fn i => i) n
        fun joinerStarContract (V:vertex seq,E:(vertex * (vertex * edge)) seq,seed) = 
            let
              val minEw = inject E (tabulate (fn _ => (~1,(0,0,0))) n)
              val minE = filter (fn (_,(v,_)) => v >= 0) (enum minEw) (*得到最小边集*)
              val coins = Rand.flip seed n
              fun head u = if (nth coins u) = 0 then true else false
              fun contract (u,(v,e)) = if (head u) andalso (not (head v)) then true else false(*star contract*) 
              val P = filter contract minE
              val Vcon = map (fn (u,(v,e)) => (u,v)) P
              val V' = inject Vcon V(*更新*)
            in
              (V',P)
            end
        fun Boruvka ((V,E),T:edge seq,i) = 
            if length E = 0 then T
            else 
                let
                  val (V',PT) = joinerStarContract (V,E,i)
                  val P = map (fn (u,(v,l)) => (nth V' u,(nth V' v,l))) E (*star*)
                  val T' = map (fn (u,(v,l)) => l) PT
                  val E' = filter (fn (u,(v,l)) => u <> v) P
                  val T'' = append (T,T')
                  val i' = Rand.next i
                in
                  Boruvka ((V',E'),T'',i')
                end
      in
        Boruvka ((vertices,edge),empty(),Rand.fromInt 1004)
      end

end
