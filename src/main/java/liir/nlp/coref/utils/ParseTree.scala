package liir.nlp.coref.utils
import scala.collection.mutable.Stack
import liir.nlp.coref.utils.structure._
class ParseTree {
  
  
  def fromString(str:String): Tree = {
    val all_words = parseString (str)
    var stack = Stack[Any]()
    for (w <- all_words){
      if (w != ")")
        stack.push(w)
        else{
          var tmp = Array[Any]()
         while ( stack.top != "(" ){
           var t = stack.pop()
           tmp :+=t
         }
          if (tmp.length > 0){
            var t : Tree=null
            tmp(tmp.length-1) match {
              case str: String => 
                 t  = new Tree(str)
                 
              case tr : Tree => t= tr
                
              
            }
            
            for (j<-(0 until tmp.length-1).reverse){
              t.addChildToTheRight(tmp(j))
            }
            stack.pop()
            stack.push(t)
          }
        }
    }
   var rs= stack.pop()
   
   require (rs.isInstanceOf[Tree])
   
   rs.asInstanceOf[Tree]
    
  }
  
  
  def parseString( str : String):Array[String] ={
   
    var all_words = Array[String]()
    var current_word = Array[Char]()  
    for (i<-0 until str.length){
      if (str(i) == ')' || str(i) == '(')
        { 
        if (current_word.length >0){
            all_words :+=current_word.map(c => c.toString).deep.mkString("")
            current_word= Array[Char]()
          }
        
        all_words:+= str(i).toString() }
        else if (str(i) == ' ' ){
          if (current_word.length >0){
            all_words :+=current_word.map(c => c.toString).deep.mkString("")
            current_word= Array[Char]()
          }
        }
        else
        {
          current_word:+=str(i)
        }
        
        
    }
    
   all_words
  }

  
  
}

object ParseTest{
  def main(args: Array[String]){
    var pt = new ParseTree()
   var tr = pt.fromString("(ROOT (S (NP (NP (CD Ten_3) (NNS months_4)) (PP (IN before_5) (NP (DT the_6) (NNP September_7) (NN operation_8)))) (, ,_9) (PP (IN in_10) (NP (NP (DT the_11) (NN city_12)) (PP (IN of_13) (NP (NNP Kandahar_14))))) (, ,_15) (NP (NNP Allah_16)) (VP (VBD blessed_17) (NP (PRP me_18)) (PP (IN with_19) (NP (DT the_20) (NN chance_21))) (S (VP (TO to_22) (VP (VB go_23) (PP (TO to_24) (NP (DT the_25) (NN frontline_26) (S (VP (TO to_27) (VP (VB fight_28) (PP (IN against_29) (NP (NP (NNP Ahmad_30) (NNP Shah_31) (NNP Mas`ud_32) (POS 's_35)) (NN army_36))) (SBAR (IN whereas_37) (S (NP (JJ many_38) (NN mujahidin_39)) (VP (VBD remained_40) (PRT (RP behind_41)) (PP (IN in_42) (NP (DT the_43) (NN training_44) (NNS camps_45))) (PP (IN around_46) (NP (DT the_47) (NN country_48))))))))))))))) (. ._49)))")
   println(tr.getAllLeafs().length)
   println (tr.getAllLeafs().deep.mkString("\t"))
   println(tr.childs(0).childs(0).getAllLeafs().deep.mkString("\t"))
      println(tr.childs(0).childs(1).getAllLeafs().deep.mkString("\t"))
         println(tr.childs(0).childs(2).getAllLeafs().deep.mkString("\t"))
                  println(tr.childs(0).childs(3).getAllLeafs().deep.mkString("\t"))
                           println(tr.childs(0).childs(4).getAllLeafs().deep.mkString("\t"))
                                    println(tr.childs(0).childs(5).getAllLeafs().deep.mkString("\t"))





  
  }
}