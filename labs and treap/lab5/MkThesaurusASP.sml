functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NYI

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  (*用asp里的图表示thesaurus numword = numvertex synonyms = outneighbors*)
  type thesaurus = ASP.graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
     let
       val edge:(string*string) seq seq = map (fn (a,b) => map (fn c => (a,c)) b) S 
     in
       ASP.makeGraph (flatten edge)
     end

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    ASP.outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    let
      val aspp = ASP.makeASP T w1 
    in
      ASP.report aspp w2
    end

end
