functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq
	   
  exception NotYetImplemented

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val maxk = KS.maxK stats
      (*选随机数作为下一个单词的p*)
      fun next (sentence,i) = 
          let  
            val kgram = drop (sentence,Int.max(0,(length sentence - maxk)))
            val histogram = KS.lookupExts stats kgram 
            val nword = if (length histogram = 0) 
                        then 
                            let
                              val kgram_edge = take (kgram,(length kgram)-1)
                              val histogram_edge = KS.lookupExts stats kgram_edge
                             in
                               Util.choose (histogram_edge) i
                             end
                        else Util.choose (histogram) i
            (*val nsentence = append(sentence,singleton nword)*)
          in
            append (sentence, singleton nword)
          end
      val ranseq = R.randomRealSeq seed NONE n
      val result = iter next (empty()) ranseq
    in
      (String.concatWith " " (toList result)) ^ "."(*加句号*)
    end	 

  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val senlen = R.randomIntSeq seed (SOME(5,10)) n(*句子长度*)
      val seedseq = R.randomIntSeq seed NONE n(*random sentence的种子*)
      fun new i =
        let
          val len = nth senlen i
          val nseed = R.fromInt (nth seedseq i)
        in
          randomSentence stats len nseed
        end
      val doc = tabulate new n
    in
      String.concatWith " " (toList doc)
    end

end
