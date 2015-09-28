package liir.nlp.sources.bekerley.interfaces;

import edu.berkeley.nlp.entity.Driver;
import edu.berkeley.nlp.entity.coref.DocumentGraph;
import edu.berkeley.nlp.entity.coref.OrderedClustering;
import liir.nlp.core.representation.Text;
import liir.nlp.coref.ilp.MyILP;
import liir.nlp.coref.io.PreprocessingClient;
import liir.nlp.coref.io.SRLClient;
import liir.nlp.coref.process.LiirDriver;
import liir.nlp.interfaces.preprocessing.Processor;
import liir.nlp.sources.bekerley.coref.DriverExtended;
import scala.Array;
import scala.Int;
import scala.Tuple2;
import scala.math.Ordering;
import scala.collection.JavaConversions;
/**
 * Created by quynhdo on 28/09/15.
 */
public class LiirCoref extends Processor {

    LiirDriver ld ;

    public LiirCoref(String modelPath, String genderPath, int w, String ilp_config) {
        super("Bekerley Coreference Resolution");
        DriverExtended de= new DriverExtended(modelPath, genderPath);

        ld = new LiirDriver(de, w, ilp_config);

    }

    public Text processToText(Text txt){

        ld.processToText(txt);
        return txt;
    }


    public void setUsingGold(boolean t){

        ld.berDriver().useGoldMentions =t;

    }
}
