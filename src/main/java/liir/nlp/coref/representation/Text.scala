package liir.nlp.coref.representation
import edu.berkeley.nlp.entity.coref.Mention

import java.io._
import liir.nlp.coref.representation.OrderingClusters

import scala.collection.mutable.HashMap
class Text  {
  var id: String  = null
  var part: String = null
  var sentences = Array[Sentence] ()
  
  var predicted_mentions = Seq[edu.berkeley.nlp.entity.coref.Mention]() 
  
  var mention_clusters = new OrderingClusters()
  
  
  
  var gold_mentions = Seq[Mention]()
  var gold_mention_to_cluster = HashMap[Int,Int]()
  
  def append (s : Sentence) {
      sentences :+=s
    }
    
  def get (i : Int):Sentence = sentences (i)
  def length = sentences.length
  
  
  

  
  def readPredictedMentionFromBerkeley(mentions: Seq[edu.berkeley.nlp.entity.coref.Mention]){ 
    predicted_mentions = mentions
    setMentionsToSentences()
  }
  
  def mentionsOfSentence(senId : Int): Seq[edu.berkeley.nlp.entity.coref.Mention]=predicted_mentions.filter { x => x.sentIdx == senId }
  
  def setMentionsToSentences(){
    sentences.map { s => s.predicted_mentions = predicted_mentions.filter { x => x.sentIdx == sentences.indexOf(s) }}
  }
  
  def setCorefClusters(clusters:Array[MentionCluster]){
    mention_clusters.setClusters(clusters)
    mention_clusters.removeSingleton()
    mention_clusters.doOrdering()
  }
  
  
  
  // read gold mentions 
  
  def readGoldMentions(){
    var totalMention=0
    for (i<-0 until sentences.length){
      var s= sentences(i)
      for (c <- s.coref_chunk){
        var m = new Mention (i, c.start_id, c.end_id, -1)
        m.id = totalMention
        totalMention +=1        
        gold_mentions :+= m
        gold_mention_to_cluster.put(m.id, c.label)
      }
      s.coref_chunk = null
    }
  }
  
  
  
  
  
  
  
  def writeMentions(output: String){
    val writer = new PrintWriter(new File(output))
      for (m<- predicted_mentions){
        writer.write(m.mentIdx.toString+ "\t" + m.sentIdx.toString +"\t" + m.startIdx.toString() + "\t" + m.endIdx.toString() + "\n")
      }
      
      writer.write("\n")
      
    
    writer.close()
    
  }
  
  def writeCoref(output: String){
    val writer = new PrintWriter(new File(output))
    var str = Array[Array[String]]()
    for (i <- 0 until sentences.length){
      val se = sentences(i)
      var ne_str = Array[String]()
      for (j<- 0 until se.words.length){
        ne_str:+=""
      }
      str:+=ne_str
      
    }
    
    val clusters = mention_clusters.clusters
    for (i<- 0 until clusters.length){
      val c = clusters(i)
      for (m <- c.mentions){
        val sId = predicted_mentions(m).sentIdx
        val startId= predicted_mentions(m).startIdx
        val endId= predicted_mentions(m).endIdx
        if (str(sId)(startId) !="")  str(sId).update(startId, str(sId)(startId) + "|" + "(" + i.toString())
        else str(sId).update(startId, "(" + i.toString())
        
        if (str(sId)(endId-1) !="")  str(sId).update(endId-1, str(sId)(endId) + "|" + i.toString() + ")")
        else str(sId).update(endId-1, i.toString() +  ")" )
          
      }
    }
    
    for (i <- 0 until sentences.length){
      val se = sentences(i)
      for (j<- 0 until se.words.length){
        writer.write(j.toString() + "\t")
        writer.write(se.words(j).form + "\t")
        writer.write(str(i)(j)+"\n")
        
      }
      
      writer.write("\n")
      
    }
    writer.close()
    
  }
  
  
  
  def writeGoldCoref(output: String){
    println(output)
    val writer = new PrintWriter(new File(output))
    writer.write("#begin document (" + id + "); part " + part + "\n" )
    var str = Array[Array[String]]()
    for (i <- 0 until sentences.length){
      val se = sentences(i)
      var ne_str = Array[String]()
      
     
      
      for (j<- 0 until se.words.length){
        ne_str:+=""
      }
      
      for (coref_chunk <- se.coref_chunk)
      {
        
        
        if (ne_str(coref_chunk.start_id) != "")
        ne_str.update (coref_chunk.start_id, ne_str(coref_chunk.start_id) + "|(" + coref_chunk.label)
        else
        ne_str.update (coref_chunk.start_id,  "(" + coref_chunk.label)
        
         if (ne_str(coref_chunk.end_id-1) != "")
         {
           if (coref_chunk.end_id-1 == coref_chunk.start_id)
                     ne_str.update (coref_chunk.end_id -1, ne_str(coref_chunk.end_id-1)  + ")")
                     else

        ne_str.update (coref_chunk.end_id -1, ne_str(coref_chunk.end_id-1) + "|" + coref_chunk.label + ")")
         }
        else
        ne_str.update (coref_chunk.end_id-1,    coref_chunk.label + ")")
        
        

      }
       
      str:+=ne_str
      
    }
    
    var all_pred_arguments = Array[Array[Array[String]]]()
    var all_preds = Array[Array[Int]]()
    var all_predsenses = Array[Array[String]]()
    for (i <- 0 until sentences.length){
      val se = sentences(i)
       var pred_arguments = Array[Array[String]]()
    var preds = Array[Int]()
    var predsenses = Array[String]()
  
     
      for (pred <- se.const_predicates){
        preds :+= pred.start_id
        predsenses :+= pred.sense
        var args = se.words.map { w => "*" }.toArray
        for (arg<- pred.args_map.keySet)
        {
          args.update(arg._1, "(" + pred.args_map(arg) + "*")
          args.update(arg._2-1, args(arg._2-1) + ")")
        }
        pred_arguments :+= args
        
        
      }
      
      all_pred_arguments :+=pred_arguments
      all_preds :+=preds
      all_predsenses:+=predsenses
     
    }
   
    
    for (i <- 0 until sentences.length){
      val se = sentences(i)
      var pred_id = 0
      for (j<- 0 until se.words.length){
        writer.write(id + "\t" + part + "\t")

        writer.write(j.toString() + "\t")
        writer.write(se.words(j).form + "\t")
     //   writer.write("\n")
        
        
        writer.write(se.words(j).pos + "\t")
        
        if (all_preds(i) contains j ){
          writer.write(all_predsenses(i)(pred_id).split("\\.")(0) + "\t" + all_predsenses(i)(pred_id).split("\\.")(1) + "\t")
          pred_id+=1
          }
          else
            writer.write("-" + "\t" + "-" + "\t")
            
            
          for (k<-0 until all_preds(i).length){
            writer.write(all_pred_arguments(i)(k)(j) + "\t")
          }
          if (str(i)(j)!="")
        writer.write(str(i)(j)+"\n")
        else
                  writer.write("-\n")

        
      }
      
      writer.write("\n")
      
    }
        writer.write("#end document")

    writer.close()
    
  }
  
  def writeCorefSimple(output:String){
    val writer = new PrintWriter(new File(output))
    var clusters = HashMap[Int,Array[Tuple3[Int, Int, Int]]]()
    for (i <- 0 until sentences.length){
     var  se = sentences(i)
      for (c <- se.coref_chunk){
        
        if (clusters.keySet contains c.label)
        {
          clusters.update(c.label, clusters(c.label):+(new Tuple3[Int,  Int, Int](i, c.start_id, c.end_id)))
        }
        else
        {
          clusters.put(c.label, Array[Tuple3[Int, Int, Int]]((i, c.start_id, c.end_id)))
        }
      
      }
  
      }
    
    
    for (k <- clusters.keys){
    
      for (m<- clusters(k)){
      
     val str= sentences(m._1).words.filter { w => (sentences(m._1).words.indexOf(w) >= m._2)  && (sentences(m._1).words.indexOf(w) < m._3)}.map { w => w.form }.toArray.deep.mkString(" ")
      writer.write(k.toString + "\t" + m._1.toString + "\t" + m._2.toString + "\t"+ m._3.toString + "\t" + str+ "\n"  )
      
      }
      writer.write("\n")
      
    }
    
    writer.close()
    
    
  }
  
  
  
  
}

