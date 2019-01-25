functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  type 'a stseq = 'a STSeq.stseq
  open Seq

  exception NYI

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph = 
      let
        val dir = map (fn (a,b) => (b,a)) E
        val directed = append(E,dir)
        val ord = collect Int.compare directed(*lable*)
      in
        map (fn (a,b) => b) ord
      end
  (*Tarjan算法 有向图的强连通算法 time stamp 
  DFN[ i ] : 在DFS中该节点被搜索的次序(时间戳)
  LOW[ i ] : 为i或i的子树能够追溯到的最早的栈中节点的次序号
  当DFN[ i ]==LOW[ i ]时，为i或i的子树可以构成一个强连通分量*)
  fun findBridges (G : ugraph) : edges = 
      let
        val numv = length G
        val vertices = tabulate (fn i => i) numv
        val start = STSeq.fromSeq (tabulate (fn _ => NONE) numv)
        val maxlow = numv * numv

        fun dfs((X:int option STSeq.stseq, bridges:edge seq, dfn, low, u:vertex), v:vertex)=
            case STSeq.nth X v of
            SOME stamp =>(X, bridges, dfn, Int.min(low,stamp),u)(*已访问*)
            |NONE =>
                let 
                  val newX = STSeq.update(v,SOME dfn) X
                  val unvisited = filter (fn x => if x = u then false else true) (nth G v)(*discover*) (*如果v的邻居里有u 去掉u 避免u-v-u*)
                  val (X2,nbridge,ndfn,nlow,_) = iter dfs (newX,bridges,dfn+1,maxlow,v) (unvisited) 
                  val finalb = if (nlow >= dfn) then append(nbridge,singleton(u,v)) else nbridge(*Tarjan*)
                  val finall = Int.min(low,nlow)(*最早的stamp*)
                in 
                  (X2,finalb,ndfn+1,finall,u)(*finish*)
                end
        val res = iter dfs (start,empty(),0,maxlow,~1) vertices   
        val resb = #2 res  
      in
        filter (fn (a,b) => a <> ~1) resb (* (~1,0) *)
      end
end
