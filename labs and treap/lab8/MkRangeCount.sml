functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = (point table) table
  (* Remove this line before submitting! *)
  exception NYI

  fun makeCountTable (S : point seq) : countTable =
      if Seq.length S = 0 then empty()
      else
        let
            val sorted = Seq.sort (fn (x,y) => compareKey (#1 x,#1 y)) S(*按横坐标排序*)
            val totablev = Seq.map (fn (x,y) => (y,(x,y)) ) sorted 
            val totablek = Seq.map (fn (x,y) => x) sorted
            fun insertion (a,b) = insert (fn (x,y) => y) b a (*a sweep line 0~x内的点*)
            val res = Seq.iterh insertion (empty()) totablev(*类似scanI*)
            val resultv = Seq.append(Seq.drop (#1 res,1),Seq.singleton (#2 res))
            val kv = Seq.zip totablek resultv
        in
            fromSeq kv
        end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int = 
      let
        val xrange = getRange T (xLeft,xRght)
        val point_r = 
            case last xrange of
            NONE => 0
            |SOME (x,y) => size (getRange y (yLo,yHi))
        val point_l = 
            case first xrange of
            NONE => 0
            |SOME (x,y) => size (getRange y (yLo,yHi))
        val num = point_r - point_l + 1 
      in
        if (size xrange = 0) then 0 else num
      end
end