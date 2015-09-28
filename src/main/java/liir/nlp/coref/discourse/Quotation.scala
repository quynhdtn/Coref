package liir.nlp.coref.discourse
import liir.nlp.coref.representation._
import edu.berkeley.nlp.entity.coref.Number
import scala.collection.mutable.ArrayBuffer
class Quotation (val open_quote: (Int,Int), val close_quote: (Int,Int)){

  var predicate :Tuple2[Predicate[(Int,Int,Int)],Int] = null
  var predicate_lemma = ""
  var subject_id = -1
  var object_id = -1
  
  var object_in_quote_id = -1
  
  var mentions = Array[Int]()
  var subject_chain = Seq[Int]()
  var object_chain = Seq[Int]()
  var words = ArrayBuffer[Word]()
  
  var same_clusters =Array[(Int,Int)]()
  var different_clusters =Array[(Int,Int)]()
  
  
  
  
  def subjectChain(txt: Text) {
    
    if (subject_id != -1){
    if (txt.predicted_mentions(subject_id).number == Number.PLURAL)
         subject_chain = mentions.toSeq.filter { m => Set("we", "our", "ours", "ourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase }
    else
     subject_chain =  mentions.toSeq.filter { m => Set("i", "me", "my", "mine", "myself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase }
    }
       
  }
  
  def objectChain(txt: Text) {
     if (object_id != -1){
       object_chain = mentions.toSeq.filter { m => Set("you", "your", "yours", "yourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase }
     }
  }
  
  def exportConstraints(txt: Text){
     if (subject_id != -1){
         
         for (m <- mentions){ 
           
           if (Set("i", "me", "my", "mine", "myself" ) contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
                same_clusters :+= makePair(m, subject_id)
    
           else     if (Set("we", "our", "ours", "ourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
               {
               if (txt.predicted_mentions(subject_id).number == Number.PLURAL)
                              same_clusters :+= makePair(m,subject_id)
                              
               }
               
               else{
                 different_clusters :+= makePair(m,subject_id)
               }

             
          
         
         }
       }
     
     
       for (mid <- 0 until mentions.length -1){ 
         for (nid <- mid +1 until mentions.length) {
          // if ((Set("i", "me", "my", "mine", "myself" ) contains  txt.predicted_mentions(mentions(mid)).words.toArray.deep.mkString(" ").toLowerCase)  && (Set("you", "your", "yours", "yourself", "he", "his","him","himself", "she", "her", "herself") contains  txt.predicted_mentions(mentions(nid)).words.toArray.deep.mkString(" ").toLowerCase))
          if ((Set("i", "me", "my", "mine", "myself" ) contains  txt.predicted_mentions(mentions(mid)).words.toArray.deep.mkString(" ").toLowerCase)  &&
              (! (Set("i", "me", "my", "mine", "myself")  contains  txt.predicted_mentions(mentions(nid)).words.toArray.deep.mkString(" ").toLowerCase)))
          
           {
             different_clusters :+= makePair(mentions(mid),mentions(nid))
           }
           
          
         }
         
       }
     
     
     if (object_id != -1){
         
         for (m <- mentions){ 
           
           if (Set("you", "your", "yours", "yourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
                same_clusters :+= makePair(m, object_id)
                
                
                
        //    if (Set("i", "me", "my", "mine", "myself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
          //      different_clusters :+= makePair(m, object_id)
    
         }
         
         if (object_in_quote_id != -1)
           same_clusters :+= makePair(object_in_quote_id, object_id)
     }
     
     
     if (object_in_quote_id != -1)
       
       if (object_id!=1){
         same_clusters :+= makePair(object_id, object_in_quote_id)
       }
       else
        for (m <- mentions){ 
           
           if (Set("you", "your", "yours", "yourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
                same_clusters :+= makePair(m, object_in_quote_id)
    
         }
         
     subjectChain(txt)
     objectChain(txt)
  
  }  
  
  /*
  def exportConstraints(txt: Text){
     if (subject_id != -1){
         
         for (m <- mentions){ 
           
           if (Set("i", "me", "my", "mine", "myself" ) contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
                same_clusters :+= makePair(m, subject_id)
    
           else     if (Set("we", "our", "ours", "ourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
               {
               if (txt.predicted_mentions(subject_id).number == Number.PLURAL)
                              same_clusters :+= makePair(m,subject_id)
                              
               }
               
               else{
                 different_clusters :+= makePair(m,subject_id)
               }

             
          
         
         }
       }
     
     
       for (mid <- 0 until mentions.length -1){ 
         for (nid <- mid +1 until mentions.length) {
          // if ((Set("i", "me", "my", "mine", "myself" ) contains  txt.predicted_mentions(mentions(mid)).words.toArray.deep.mkString(" ").toLowerCase)  && (Set("you", "your", "yours", "yourself", "he", "his","him","himself", "she", "her", "herself") contains  txt.predicted_mentions(mentions(nid)).words.toArray.deep.mkString(" ").toLowerCase))
          if ((Set("i", "me", "my", "mine", "myself" ) contains  txt.predicted_mentions(mentions(mid)).words.toArray.deep.mkString(" ").toLowerCase)  &&
              (! (Set("i", "me", "my", "mine", "myself")  contains  txt.predicted_mentions(mentions(nid)).words.toArray.deep.mkString(" ").toLowerCase)))
          
           {
             different_clusters :+= makePair(mentions(mid),mentions(nid))
           }
           
          
         }
         
       }
     
     
     if (object_id != -1){
         
         for (m <- mentions){ 
           
           if (Set("you", "your", "yours", "yourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
                same_clusters :+= makePair(m, object_id)
                
                
                
        //    if (Set("i", "me", "my", "mine", "myself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
          //      different_clusters :+= makePair(m, object_id)
    
         }
         
         if (object_in_quote_id != -1)
           same_clusters :+= makePair(object_in_quote_id, object_id)
     }
     
     
     if (object_in_quote_id != -1)
       
       if (object_id!=1){
         same_clusters :+= makePair(object_id, object_in_quote_id)
       }
       else
        for (m <- mentions){ 
           
           if (Set("you", "your", "yours", "yourself") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase )
                same_clusters :+= makePair(m, object_in_quote_id)
    
         }
         
  
  }     
  
  */
  
  def connectToReply(q: Quotation){
    if (q.subject_id != -1){
      if (object_id == -1){
        object_id = q.subject_id
      }
    }
 
    if (q.subject_id == -1){
      if (object_id != -1){
        q.subject_id = object_id
      }
    }
 
    
    if (q.object_id == -1){
     
      if (subject_id != -1)
        q.object_id= subject_id
      }
  }
  def makePair (m1: Int, m2: Int): (Int,Int)={
    if (m1 == -1 || m2 == -1)
      return null
     if (m1 < m2)
          return   (m1,m2)
      else  if (m2 < m1)
         return   (m2,m1)
       null
  }
    
}