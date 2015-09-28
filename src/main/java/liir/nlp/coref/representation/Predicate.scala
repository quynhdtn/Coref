package liir.nlp.coref.representation
import scala.collection.mutable.HashMap


case class Predicate[T](val start_id: Int, val end_id: Int){
    
    
    var args_map = HashMap[T , String]()   // argument map: from argument id to Label
    var sense : String = null
    
    def addArgument( arg: T, lbl : String)= {      
      args_map += arg -> lbl
    }
    
    def getArgument(arg: T) = args_map(arg)
    
    def isArgument(c: T) = args_map.exists(_._1 ==c)
    
    
    
    
    def isCoArgument(c1: T, c2: T) = args_map.exists(_._1 ==c1) && args_map.exists(_._1 ==c2)
    
  
}


/*
class Predicate[Tuple2[Int, Int]] (var start_id: Int, var en_id: Int){



 def inConstituent( arg_start: Int, arg_end: Int , se:Sentence): Boolean ={
   
     for (k <- args_map.keysIterator){
       if ( k._1 == arg_start &&   k._2 == arg_end-1) return true 
        else
        {
          if (se.words(k._1).pos == "IN")
            if ((k._1 == arg_start-1) && (k._2 == arg_end-1))
            return  true
        }
        
     }
    false 
 }
   
    def getSemanticLabel(arg_start: Int, arg_end: Int , se:Sentence): String ={
   
       
       for (k <- args_map.keysIterator){
         if ( k._1 == arg_start &&   k._2 == arg_end-1) return args_map(k)
          else
          {
            if (se.words(k._1).pos == "IN")
              if ((k._1 == arg_start-1) && (k._2 == arg_end-1))
              return  pred.args_map(k)
          }
          
       }
       
       return ""
        
    }




}
*/