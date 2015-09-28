package liir.nlp.coref.representation

/**
 * @author quynhdo

 */
class Word {
  
  var id :Int = 0
  var form : String = null
  var lemma: String = null
  var pos: String = null
  var head: Int = -1
  var deprel: String = null
  var parse_bit: String = null
  var word_sense: String = null 
  var ne_label: String  = null   // ne label of the word in Parenthese form
  var coref_label: String = null // coref label of the word in Parenthese form
  var chunk: String   = null     // chunking label of the word in Parenthese form
  var speaker : String = null
  
  
}


  
