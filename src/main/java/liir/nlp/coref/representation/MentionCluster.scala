package liir.nlp.coref.representation


// this class represent a cluster of entities
class MentionCluster {
    
  var id: Int = 0
  var mentions=Seq[Int]() // sequence of mention ids
  
  
  def addMention(mId : Int){ mentions:+=mId ; mentions = mentions.sorted }
  
  def getAntecedents(mId: Int) = mentions.filter { m => m < mId }
  
  def isSingleton() =  mentions.length == 0
  
  def getMention(i: Int)=  mentions(i)
  
  def getBestScoreToCluster (mId: Int, score_chart: Array[Array[Float]]) : Float={
    var scores = mentions.filter { x => x < mId }.map { x => score_chart(mId)(x) }
    scores.reduceLeft((v1,v2) => if (v1 > v2) v1 else v2)    
  }
  
  override def toString():String = {
    mentions.toArray.deep.mkString("\t")
   
  }
  
  def getBackPt(mId: Int):Int={
    if (mentions(0)==mId) mId
    else
   
     mentions(mentions.indexOf(mId)-1)
  }
  
}

class OrderingClusters{
  
  var clusters= Seq[MentionCluster]()
 
  def addCluster(c : MentionCluster) {clusters :+= c}
  
  def getCluster(i : Int) = clusters(i)
  
  def setClusters(allc : Array[MentionCluster]) { clusters = allc}
  
  def doOrdering(){
    
    clusters = clusters.sortWith(_.getMention(0) < _.getMention(0)) }
  
  def removeSingleton(){
    
    clusters = clusters.filter { c => c.mentions.length > 1} }
  
  
}