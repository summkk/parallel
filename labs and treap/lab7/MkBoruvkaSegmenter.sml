functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Remove this exception when you're done! *)
  exception NotYetImplemented

  fun findSegments (E, n) initial_credit =
      let
        fun cmp ((u1,v1,w1),(u2,v2,w2)) = Int.compare (w1,w2)
        val sorte = rev (sort cmp E)(*转换为从大到小*)
        val edge = map (fn (u,v,w) => (u,(v,w))) sorte
        val vertices = tabulate (fn i => i) n
        val credits = tabulate (fn _ => initial_credit) n

        fun joinerStarContract (V,E) seed =
            let
              val minEw = inject E (tabulate (fn _ => (~1,~1)) n)
              val minE = filter (fn (_,(v,_)) => v >= 0) (enum minEw)
              val coins = Rand.flip seed n
              fun contract (u,(v,e)) = if (nth coins u) = 0 andalso (nth coins v) = 1 then true else false
              val P = filter contract minE 
              val Vcon = map (fn (u,(v,e)) => (u,v)) P
              val V' = inject Vcon V
            in
              (V',P)
            end

        fun Boruvka ((V,E,c),seed) =
            if length E = 0 then V
            else
              let
                val (V',PT) = joinerStarContract (V,E) seed
                val Vfinal = map (fn v => nth V' v) V'
                (*credit inject then filter*)
                val to_collect = map (fn (u,(v,w)) => (v,(u,w))) PT 
                val ngcontract = collect Int.compare to_collect
                fun sum (v:vertex,s:(vertex * weight) seq) = (v,reduce op+ 0 (map #2 s))
                val nsum = map sum ngcontract(*the sum of the weights of the contracted edges*)
                fun cre (v,s:(vertex * weight) seq) = reduce Int.min initial_credit (map (fn (v,w) => nth c v) s)
                val minc = map (fn (v,s) => (v,Int.min(nth c v, cre(v,s)))) ngcontract(*找到endpoints里的最小credit*)
                val nc = inject minc c
                val final = map (fn (v,w) => (v,((nth nc v) - w))) nsum
                val finalc = inject final nc 
                val P = map (fn (u,(v,w)) => (nth Vfinal u,(nth Vfinal v,w))) E
                fun tofil (u,(v,w)) = (u <> v) andalso (w < (nth finalc u) andalso (w < (nth finalc v)))
                val E' = filter tofil P 
                val seed' = Rand.next seed
              in
                Boruvka((Vfinal,E',finalc),seed')
              end
      in
        Boruvka((vertices,edge,credits),Rand.fromInt 528)
      end
end
