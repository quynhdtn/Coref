package liir.nlp.coref.representation

import edu.berkeley.nlp.entity.DepConstTree
import liir.nlp.coref.utils.Dictionary
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.coref.MentionType
import liir.nlp.coref.utils.structure._
class Sentence {  
  
    var id : Int = 0
    var part : Int = 0
    var ne_chunk : ArrayBuffer[Chunk[String]] = null
    var coref_chunk = ArrayBuffer[Chunk[Int]]()
    var const_predicates= ArrayBuffer[Predicate[Tuple3[Int,Int,Int]]]()  //srl in constituent style
    var dep_predicates: ArrayBuffer[Predicate[Int]] = null // srl in dependency style
    var words = ArrayBuffer[Word] ()
    var dep_const_tree : DepConstTree = null
    var predicted_mentions= Seq[edu.berkeley.nlp.entity.coref.Mention] ()
    var const_tree : Tree = null
    var docid:String=null
   
    
    def append (w : Word) {
      words:+=w
    }
    
    def get (i : Int):Word = words (i)
    def length = words.length
    
    def getPronMentions()= predicted_mentions.filter { m => m.mentionType == MentionType.PRONOMINAL}
    def getNonPronMentions() = predicted_mentions.filter { m => m.mentionType != MentionType.PRONOMINAL}
    
    def getMentions = predicted_mentions
    
    def getMentionIds = predicted_mentions.map { m => m.mentIdx }
    
    def getPronMentionIds()= predicted_mentions.filter { m => m.mentionType == MentionType.PRONOMINAL}.map { m => m.mentIdx }
    def getNonPronMentionIds() = predicted_mentions.filter { m => m.mentionType != MentionType.PRONOMINAL}.map { m => m.mentIdx }
    
    def checkMentionInSentence(mId: Int):Boolean = {predicted_mentions.exists { x => x.mentIdx == mId }}
    
    // get the predicate of a mention and their role
    def getPredicatesOfMention(m: edu.berkeley.nlp.entity.coref.Mention) = {
      var rs = Array[Tuple2[Predicate[(Int,Int,Int)], String]]()
      for (pred <- const_predicates){
        if (pred.isArgument(m.startIdx, m.endIdx, m.headIdx))
          rs :+= (pred, pred.getArgument(m.startIdx, m.endIdx, m.headIdx))
      }
      rs
    }
    
    def checkPronChunk(c: Chunk[Int]):Boolean = {
  //    println(c.start_id + "\t" + c.end_id)
   //   println(toString())
      c.start_id == c.end_id-1 && words(c.start_id).pos.startsWith("PRP")
    }
    def hasPronChunk: Boolean = coref_chunk.exists { x => checkPronChunk (x)}
    
    
     override def toString(): String = words.map { w => w.form }.toArray.deep.mkString(" ")
     
     
     // find ids of the predicates that have the given mention as argument
    def findFrames (arg_start: Int, arg_end: Int): Seq[Int]={
      var rs = Seq[Int]()
      for (i <- 0 until const_predicates.length){
        var pred = const_predicates(i) 
    //    print(words(pred.start_id).form.toLowerCase)
        if (! (pred.sense.startsWith("be")))
   //     println(pred.sense.split("\\.")(0))
       if(!Dictionary.REFLEXIVE_VERBS.contains(pred.sense.split("\\.")(0))){
        for (k <- pred.args_map.keysIterator){
           if ( k._1 == arg_start &&   k._2 == arg_end-1)  rs :+= i
           else if (k._1 == arg_start &&   k._2 == arg_end && words(k._2).pos == ",") {
             rs :+= i
           }
            else
            {
              if (words(k._1).pos == "IN"|| words(k._1).pos == "TO")
                  if (words(k._1).form.toLowerCase != "as"  )
                if ((k._1 == arg_start-1) && (k._2 == arg_end-1))
                 rs :+= i
                 
            }
     
        }
       }
      
       }
   
     rs
     }
     
     
     def findFramesHead (arg_head: Int): Seq[Int]={
      var rs = Seq[Int]()
      for (i <- 0 until const_predicates.length){
        var pred = const_predicates(i) 
    //    print(words(pred.start_id).form.toLowerCase)
        if (! (pred.sense.startsWith("be")))
   //     println(pred.sense.split("\\.")(0))
       if(!Dictionary.REFLEXIVE_VERBS.contains(pred.sense.split("\\.")(0))){
        for (k <- pred.args_map.keysIterator){
           if ( k._3 == arg_head)  rs :+= i
            else
            {
              if (words(k._1).pos == "IN"|| words(k._1).pos == "TO")
                  if (words(k._1).form.toLowerCase != "as"  )
                if ((k._3 == arg_head) )
                 rs :+= i
                 
            }
     
        }
       }
      
       }
   
     rs
     }
     
          //find the index of all the semantic frames that receive the two given mentions as arguments

     def findCoFrames (arg_start1: Int, arg_end1: Int, arg_start2: Int, arg_end2: Int): Seq[Int]=
     {
       var rs = Seq[Int]()
      val frs1= findFrames(arg_start1, arg_end1)
      val frs2= findFrames(arg_start2, arg_end2)
      
      frs1.filter { p => frs2 contains p }
   
    
     }
     
     def findCoFramesHead (arg_head1: Int, arg_head2: Int): Seq[Int]=
     {
       var rs = Seq[Int]()
      val frs1= findFramesHead(arg_head1)
      val frs2= findFramesHead(arg_head2)
      
      frs1.filter { p => frs2 contains p }
   
    
     }
     
      def findCoFramesSubjectObject (arg_start1: Int, arg_end1: Int, arg_start2: Int, arg_end2: Int): Int=
     {
         var rs = Seq[Int]()
        val frs1= findFrames(arg_start1, arg_end1)
        val frs2= findFrames(arg_start2, arg_end2)
        
        val frrs  = frs1.filter { p => frs2 contains p }
         for (i<- frrs.length-1 to 0 by -1){
           val lbl1 = getSemanticLabel(frrs(i), arg_start1, arg_end1)
           val lbl2 = getSemanticLabel(frrs(i), arg_start2, arg_end2)
           if (lbl1 != lbl2 && ( Set("A0", "A1", "ARG0", "ARG1") contains lbl1) && ( Set("A0", "A1", "ARG0", "ARG1") contains lbl2)) 
             return frrs(i)
            
               
         }
          
         return -1
     
    
     }
     
       def findCoFramesSubjectObjectHead (arg_head1: Int, arg_head2: Int): Int=
     {
         var rs = Seq[Int]()
        val frs1= findFramesHead(arg_head1)
        val frs2= findFramesHead(arg_head2)
        
        val frrs  = frs1.filter { p => frs2 contains p }
         for (i<- frrs.length-1 to 0 by -1){
           val lbl1 = getSemanticLabelHead(frrs(i),arg_head1)
           val lbl2 = getSemanticLabelHead(frrs(i), arg_head1)
           if (lbl1 != lbl2 && ( Set("A0", "A1", "ARG0", "ARG1") contains lbl1) && ( Set("A0", "A1", "ARG0", "ARG1") contains lbl2)) 
             return frrs(i)
            
               
         }
          
         return -1
     
    
     }
     
     //find the index of the last semantic frame that receive the two given mentions as arguments
     def findLastIndexCoFrames (arg_start1: Int, arg_end1: Int, arg_start2: Int, arg_end2: Int): Int=
     {
      val frs1= findFrames(arg_start1, arg_end1)
      val frs2= findFrames(arg_start2, arg_end2)
      
      val rss = frs1.filter { p => frs2 contains p }
       if (rss.length == 0) return -1
       else
         return rss( rss.length-1)
   
    
     }
     
     
     
     def getSemanticLabel( predId: Int, arg_start: Int, arg_end: Int): String = {
       var pred= const_predicates(predId)
       
       for (k <- pred.args_map.keysIterator){
         if ( k._1 == arg_start &&   k._2 == arg_end-1) return pred.args_map(k)
          else if (k._1 == arg_start &&   k._2 == arg_end && words(k._2).pos == ",") {
            println("haha")
          return  pred.args_map(k)
          }
          else
          {
            if (words(k._1).pos == "IN" || words(k._1).pos == "TO")
            
              if ((k._1 == arg_start-1) && (k._2 == arg_end-1)) 
              return  pred.args_map(k)
            
          }
          
       }
       
       return ""
     }
     
      def getSemanticLabelHead( predId: Int, arg_head: Int): String = {
       var pred= const_predicates(predId)
       
       for (k <- pred.args_map.keysIterator){
         if ( k._3 == arg_head) return pred.args_map(k)
          else
          {
            if (words(k._1).pos == "IN" || words(k._1).pos == "TO")
            
              if ((k._3 == arg_head)) 
              return  pred.args_map(k)
            
          }
          
       }
       
       return ""
     }
      
      def getSemanticLabel( pred: Predicate[Tuple3[Int,Int,Int]], arg_start: Int, arg_end: Int): String = {
       
       for (k <- pred.args_map.keysIterator){
         if ( k._1 == arg_start &&   k._2 == arg_end-1) return pred.args_map(k)
          else if (k._1 == arg_start &&   k._2 == arg_end && words(k._2).pos == ",") {
          return  pred.args_map(k)
          }

          else
          {
            if (words(k._1).pos == "IN" || words(k._1).pos == "TO")
            
              if ((k._1 == arg_start-1) && (k._2 == arg_end-1)) 
              return  pred.args_map(k)
            
          }
          
       }
       
       return ""
     }
      
      /*
      def predicateInQuote(pred: Predicate[Tuple3[Int,Int,Int]], quotes: ArrayBuffer[Quotation]): Boolean = {
        val fs = quotes.filter { q => var rs = true
          if (q.open_quote._2 == id && q.close_quote._2 == id)
            if (q.open_quote._1 <= pred.start_id && q.close_quote._1 >= pred.start_id)
              rs= false
              
          if (q.open_quote._2 == id && q.close_quote._2 > id)
            if (q.open_quote._1 <= pred.start_id )
              rs= false
              
           if (q.open_quote._2 < id && q.close_quote._2 == id)
            if (q.close_quote._1 >= pred.start_id)
              rs= false
        
        rs
        }
        
       (fs.length > 0)
        
      }
      
      
     
      def assignPredicates (quotes: ArrayBuffer[Quotation]) : ArrayBuffer[(Int,Int)] = {
        
      }
      * 
      */
    
}

