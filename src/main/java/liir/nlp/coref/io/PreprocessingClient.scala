package liir.nlp.coref.io
/**
 * @author quynhdo
 */

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
class PreprocessingClient (val address:String, val port: String) {
  
        
      def process(txt: String): String={

         var x="http://" + address+ ":" + port + "/bnlp/sens?text=" +java.net.URLEncoder.encode(txt)
        
         var content = getRestContent(x)
         
         
         x= "http://" + address+ ":" + port + "/bnlp/tokens?text=" + java.net.URLEncoder.encode(content)
        
         content = getRestContent(x)


          x= "http://" + address+ ":" + port + "/lnlp/pos?text=" + java.net.URLEncoder.encode(content)

          content = getRestContent(x)

        x= "http://" + address+ ":" + port + "/lnlp/lemma?text=" + java.net.URLEncoder.encode(content)

        content = getRestContent(x)

        x= "http://" + address+ ":" + port + "/lnlp/parse?text=" + java.net.URLEncoder.encode(content)

        content = getRestContent(x)


        x= "http://" + address+ ":" + port + "/lnlp/srl?text=" + java.net.URLEncoder.encode(content)

        content = getRestContent(x)


        x= "http://" + address+ ":" + port + "/bnlp/parse?text=" + java.net.URLEncoder.encode(content)

        content = getRestContent(x)



         content
         
      
  
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