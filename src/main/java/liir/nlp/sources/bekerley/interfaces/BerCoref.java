package liir.nlp.sources.bekerley.interfaces;

import edu.berkeley.nlp.entity.Driver;
import liir.nlp.interfaces.preprocessing.Processor;
import liir.nlp.core.representation.Text;
import liir.nlp.sources.bekerley.coref.DriverExtended;

/**
 * Created by quynhdo on 01/09/15.
 */
public class BerCoref extends Processor{

    DriverExtended de ;

    public BerCoref(String modelPath, String genderPath) {
        super("Bekerley Coreference Resolution");
        de= new DriverExtended(modelPath, genderPath);
    }

    public Text processToText(Text txt){
        de.runPrediction(txt);
        return txt;
    }


    public void setUsingGold(boolean t){
        de.useGoldMentions =t;

    }
}
