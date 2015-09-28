package liir.nlp.coref.representation

/**
 * @author quynhdo
 */

  case class Chunk[T](start_id: Int, end_id: Int, var label: T) extends Object{
  
  def compareLabel (other_chunk: Chunk[T]) = label==other_chunk.label
  
  
  }
