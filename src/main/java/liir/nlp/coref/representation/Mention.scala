package liir.nlp.coref.representation

class Mention (val sen_id : Int, val start_id: Int, val  end_id: Int, var head_id: Int) {
 
  var id: Int = -1
  var mention_type : String = null
  var gender : String = null
  var number : String = null
  
  
  def compare(m: edu.berkeley.nlp.entity.coref.Mention):Boolean = (sen_id == m.sentIdx) && (start_id == m.startIdx) && (end_id == m.endIdx)
  
}
