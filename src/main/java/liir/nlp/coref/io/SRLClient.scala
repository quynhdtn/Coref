package liir.nlp.coref.io
import java.io._
import org.apache.http.HttpEntity
import org.apache.http.HttpResponse
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DefaultHttpClient
import scala.collection.mutable.StringBuilder
import scala.xml.XML
import liir.nlp.coref.representation.Sentence


import liir.nlp.core.representation.Text

/**
 * @author quynhdo
 */
class SRLClient (val address:String, val port: String) {
  
        
      def parseSentence(s: Sentence){

        var x="http://" + address+ ":" + port + "/lnlp/srlfullxml?text=" +java.net.URLEncoder.encode(fromSentenceToTextFormat(s))
        
         val content = getRestContent(x)
         
         println(content)
         
      
  
      }

   def parse(s: String): String={

        var x = "http://" + address+ ":" + port + "/lnlp/srlfullxml?text=" +java.net.URLEncoder.encode(s)
        
         val content = getRestContent(x)
         
         content
         
      
  
      }
  
      def fromSentenceToTextFormat(s: Sentence):String={
        
        var txt = new Text()
        var se = new liir.nlp.core.representation.Sentence
        s.words.map { w => {
          
          var ww =  new liir.nlp.core.representation.Word
          ww.setStr(w.form)
          se.add(ww)
          txt.add(se)
          
          
        } }
        
        txt.toXMLString()
      }
  
  
  def getRestContent(url:String): String = {
    val httpClient = new DefaultHttpClient()
    val httpResponse = httpClient.execute(new HttpGet(url))
    val entity = httpResponse.getEntity()
    var content = ""
    if (entity != null) {
      val inputStream = entity.getContent()
      content = io.Source.fromInputStream(inputStream).getLines.mkString
      inputStream.close
    }
    httpClient.getConnectionManager().shutdown()
    return content
  }
}