functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  exception NYI

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real
  type 'a table = 'a Table.table


  (* Define this type yourself *)
  type graph = (weight table) table
  
  fun makeGraph (E : edge Seq.seq) : graph =
    let
      fun tolable (u,v,w) = (u,(v,w))
      val nedge = Table.collect (Seq.map tolable E)
    in
      Table.map Table.fromSeq nedge
    end
  (*A Simple modification of the Dijkstra algorithm*)
  fun findPath h G (S, T) = 
    let
      fun N(v) =
            case Table.find G v
              of NONE => Table.empty ()
               | SOME nbr => nbr
      fun Dijkstra (X,Q) =
          case PQ.deleteMin(Q) of
          (NONE,_) => NONE
          |(SOME(d,v),Q') =>
                if (Set.find T v) then SOME(v,d) else
                case Table.find X v of
                   SOME _ => Dijkstra (X,Q')
                   |NONE => 
                        let
                          val insert = Table.insert (fn (x,_) => x)
                          val X' = insert (v, d) X
                          (*change*)
                          fun relax (Q,(u,w)) = PQ.insert (d+w-h(v)+h(u),u) Q 
                          val Q'' = Table.iter relax Q' (N v)
                        in
                          Dijkstra (X',Q'')
                        end
      (*初始值*)
      val sources = PQ.fromList (Seq.toList (Seq.map (fn v => (0.0 + h(v),v) ) (Set.toSeq S)))
      val result = Dijkstra ((Table.empty()), sources)
    in
      result
    end
end