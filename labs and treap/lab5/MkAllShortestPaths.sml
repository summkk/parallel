functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
   (*key是点 value是邻居 邻接表 第二个int存边数 第三个int存点数*)
  type graph = ((vertex seq) table)*int*int
  (*BFS时的上一级结点*)
  type asp = vertex seq table

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
      let
           val gra = Table.collect E
           val m = length E
           val undir = map (fn (a,b) => fromList [(a,b),(b,a)]) E
           val n = size (Table.collect (flatten undir))
           (*注意只入不出的点*)
         in
           (gra,m,n)
         end   

  (* Task 2.2 *)
  fun numEdges (G : graph) : int = 
      #2 G
  

  fun numVertices (G : graph) : int =
      #3 G

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
      case Table.find (#1 G) v of
    NONE => empty()
    |SOME t => t
    
  (* Task 2.4 *)
  fun BFS (G:graph)(path:asp) (frontier:Set.set) (visited:Set.set) : asp =
      if (Set.size frontier = 0) then path 
      else
        let
          (*在frontier里找每个点outneighbor里 未被访问的点作为下一层*)
          fun next1 u = Set.difference(Set.fromSeq(outNeighbors G u),visited)  
          fun next2 u = map (fn x => (x,u)) (Set.toSeq (next1 u)) 
          val nex = map next2 (Set.toSeq frontier)
          val nasp:asp = Table.collect (flatten nex)(*正在访问的点的父节点*)
          val npath:asp = Table.merge (fn (a,b) => a) (path,nasp)
          val nfrontier = Table.domain nasp
          val nvisited = Set.union (nfrontier,visited)
        in
          BFS G npath nfrontier nvisited
        end

  fun makeASP (G : graph) (v : vertex) : asp =
      let

        val vpath = Table.singleton (v,singleton v)
        val vfrontier = Set.fromSeq (singleton v)
      in
        BFS G vpath vfrontier (Set.empty())
      end
      
  (* Task 2.5 *)
   fun report (A : asp) (v : vertex) : vertex seq seq =
    let
      fun fpath x = 
        case Table.find A x of
            NONE => Seq.empty()
            |SOME path => path
      fun DFS v : vertex seq seq =
        if length (fpath v) = 0 then empty()
        else if Table.Key.equal (nth (fpath v) 0, v) then Seq.singleton (Seq.singleton v)
        else 
          let
            val nexts_data = fpath v
            val nexts : vertex seq seq = flatten  (map DFS nexts_data)
          in
            map (fn y => append (y,Seq.singleton v)) nexts
          end
    in
      DFS v
    end
end