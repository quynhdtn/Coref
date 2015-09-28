package liir.nlp.coref.process

import java.io.{FileNotFoundException, InputStream}
import java.util
import java.util.Properties

import edu.berkeley.nlp.entity.coref.OrderedClustering
import liir.nlp.core.representation
import liir.nlp.core.representation.entities.{MentionCluster, Mention}
import liir.nlp.coref.ilp.MyILP
import liir.nlp.coref.io.PreprocessingClient
import liir.nlp.sources.bekerley.coref.DriverExtended
import liir.nlp.coref.io.SRLClient
import liir.nlp.coref.representation.Text
import liir.nlp.core.representation.io.XMLReader

import scala.collection.JavaConversions

/**
 * Created by quynhdo on 23/09/15.
 */
class Driver (berDriver: DriverExtended, preProcessor : PreprocessingClient, srlProcessor : SRLClient, w: Int, coref_prop_file:String ) {


  def process(text: String): Unit = {

    var x = preProcessor.process(text)


    println(x)
    var txt = XMLReader.readCorpus(x).get(0)

    var tmp = berDriver.getScores(txt)


    var ctxt = liir.nlp.coref.io.LiirReader.readLiirText(txt)

    ctxt.readPredictedMentionFromBerkeley(tmp._1.corefDoc.predMentions)

    var ilp = new MyILP(w, coref_prop_file)
    val rs = ilp.readProblem(tmp._2, ctxt)

    println((0 until rs._1.length).toArray.deep.mkString("\t"))
    println(rs._1.deep.mkString("\t"))
    println()
    println(rs._2.deep.mkString("\t"))

    val clustering = OrderedClustering.createFromBackpointers(rs._2)

    DriverExtended.runPredictionWithILP(txt, tmp._1, clustering)



    println(txt.toXMLString)


  }

  def processToText(text: String): representation.Text = {

    var x = preProcessor.process(text)



    var txt = XMLReader.readCorpus(x).get(0)

    var tmp = berDriver.getScores(txt)


    var ctxt = liir.nlp.coref.io.LiirReader.readLiirText(txt)

    ctxt.readPredictedMentionFromBerkeley(tmp._1.corefDoc.predMentions)

    var ilp = new MyILP(w, coref_prop_file)
    val rs = ilp.readProblem(tmp._2, ctxt)

   // println((0 until rs._1.length).toArray.deep.mkString("\t"))
   // println(rs._1.deep.mkString("\t"))
  //  println()
  //  println(rs._2.deep.mkString("\t"))

    val clustering = OrderedClustering.createFromBackpointers(rs._2)

    DriverExtended.runPredictionWithILP(txt, tmp._1, clustering)



    txt


  }

}


object Driver{


  def main(args: Array[String]): Unit = {
    val prop: Properties = new Properties
    val propFileName: String = "ber.properties"

    val inputStream: InputStream = getClass.getClassLoader.getResourceAsStream(propFileName)

    if (inputStream != null) {
      prop.load(inputStream)
    }
    else {
      throw new FileNotFoundException("property file '" + propFileName + "' not found in the classpath")
    }


    val coref_model: String = prop.getProperty("coref_model")
    val gender_path: String = prop.getProperty("gender_path")
    val w : Int= prop.getProperty("w").toInt
    val ilp_config : String = prop.getProperty("ilp_config")


    var be = new  DriverExtended(coref_model, gender_path)

    val preProcessor = new PreprocessingClient("localhost", "9999")
    val srlProcessor = new SRLClient("localhost", "9999")


    val dv = new Driver(be, preProcessor, srlProcessor,w, ilp_config)
    dv.process("We went to school yesterday. We stayed there for 4 hours. She is so happy with that. But she will return soon.")
  }

}
