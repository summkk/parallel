functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq
  exception NotYetImplemented
  type token = string
  type kgram = token seq
			
  (* You must define the abstract kgramstats type *)
  type kgramstats = int * (string hist T.table)
  (*#2 a table with keys of type string seq and values of type string hist((string * int) seq)*)
  (*取出所有长度不超过maxk的kgram*)
  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let
      val tok = tokens (not o Char.isAlphaNum) corpus 
      fun cmp (a,b) = collate String.compare(a,b) (*排序比较*) 
      
      fun find len = tabulate (fn i => (subseq tok (i,len), nth tok (len + i))) (length tok - len) (*长度len*)

    (*  fun find k = tabulate (fn i => (subseq tok (i,(k-i+1)), nth tok (k+1))) k
      val start = tabulate (fn i => i) (maxK + 1)
      val findk = flatten (map find start)*)
      val findk = flatten (tabulate find (maxK + 1))
     (* val final = filter (fn (i,s) => if (length i) > maxK then false else true) findk *)
      val tohis = collect cmp findk (*key*)
      val his = map (fn (i,s) => (i,histogram String.compare s)) tohis
    in
      (maxK,Table.fromSeq his)
    end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq = 
      case Table.find (#2 stats) kgram of
        SOME t => t
        |NONE => empty()

  fun maxK (stats : kgramstats) : int =
    #1 stats
end
    
(*fun find k = tabulate (fn i => (subseq tok (i,(k-i+1)), nth tok (k+1))) k 
        找出以i为开头以k为结尾的片段，和片段后面第一个单词组成二元组  bug*)