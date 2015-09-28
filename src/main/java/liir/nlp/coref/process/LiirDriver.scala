package liir.nlp.coref.process

import java.io.{FileNotFoundException, InputStream}
import java.util.Properties

import edu.berkeley.nlp.entity.coref.OrderedClustering
import liir.nlp.core.representation
import liir.nlp.core.representation.Text
import liir.nlp.core.representation.io.XMLReader
import liir.nlp.coref.ilp.MyILP
import liir.nlp.coref.io.{SRLClient, PreprocessingClient}
import liir.nlp.coref.process.Driver._
import liir.nlp.sources.bekerley.coref.DriverExtended

/**
 * Created by quynhdo on 28/09/15.
 */
class LiirDriver
   (var berDriver: DriverExtended,  w: Int, coref_prop_file:String ) {




    def processToText(txt: Text):Text = {


      var tmp = berDriver.getScores(txt)


      var ctxt = liir.nlp.coref.io.LiirReader.readLiirText(txt)

      ctxt.readPredictedMentionFromBerkeley(tmp._1.corefDoc.predMentions)

      var ilp = new MyILP(w, coref_prop_file)
      val rs = ilp.readProblem(tmp._2, ctxt)



      val clustering = OrderedClustering.createFromBackpointers(rs._2)

      DriverExtended.runPredictionWithILP(txt, tmp._1, clustering)

      txt

    }








}
