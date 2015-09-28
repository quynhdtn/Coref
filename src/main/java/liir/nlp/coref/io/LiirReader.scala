package liir.nlp.coref.io
import liir.nlp.coref.representation.Text
import liir.nlp.coref.representation.Sentence
import liir.nlp.coref.representation.Word
/**
 * @author quynhdo
 */
object LiirReader {
  
  
  
  def readLiirText(source: liir.nlp.core.representation.Text): Text={
    
    var text = new Text()
    for (i <- 0 until source.size){
      var sen = source.get(i)
      var se = new Sentence()
      for (j <- 0 until sen.size){
        var w = new Word
        w.id = sen.get(j).getId.toInt
        w.form = sen.get(j).getStr
        w.lemma = sen.get(j).getLemma
        w.pos = sen.get(j).getPos
        w.head = sen.get(j).getHead.toInt
        w.deprel = sen.get(j).getDeprel
        se.append(w)
        
      }
      text.append(se)
      
    }
    
    text
    
    
  }
  
}