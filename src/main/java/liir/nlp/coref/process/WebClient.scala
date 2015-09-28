package liir.nlp.io
import java.io._
import liir.nlp.coref.io.PreprocessingClient
import org.apache.http.HttpEntity
import org.apache.http.HttpResponse
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DefaultHttpClient
import scala.collection.mutable.StringBuilder
import scala.xml.XML
import liir.nlp.coref.io.SRLClient
object Wclient{
  
 def preprocess(txt: String){
   
 }
 
 
 def main(args: Array[String]) {
   
   
 var txt="I love you so much. But you do not like me."
 
 var cl =new  SRLClient("localhost", "9999")
 var pl = new PreprocessingClient("localhost", "9999")
 var x =pl.process(txt)
 cl.parse(x)
 
   
    // (1) get the content from the yahoo weather api url
 
 /*
 var u = "http://localhost:9999/bnlp/sens?text=" + txt
 var x="http://localhost:9999/bnlp/sens?text=" +java.net.URLEncoder.encode(txt)
 println(x)
    val content = getRestContent(x)
     
    // (2) convert it to xml
    val xml = XML.loadString(content)
    assert(xml.isInstanceOf[scala.xml.Elem])  // needed?
 
   println(xml)
   * 
   * 
   */
  }
   
  /**
   * Returns the text content from a REST URL. Returns a blank String if there
   * is a problem.
   */
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