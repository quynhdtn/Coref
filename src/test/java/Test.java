import edu.berkeley.nlp.entity.ConllDoc;
import liir.nlp.io.XMLReader;
import liir.nlp.representation.Text;
import liir.nlp.sources.bekerley.coref.Conversion;
import liir.nlp.sources.bekerley.coref.DriverExtended;
import liir.nlp.sources.bekerley.interfaces.BerParser;
import liir.nlp.sources.bekerley.interfaces.BerSentenceSplitter;
import liir.nlp.srl.sources.lth.interfaces.LundParser;
import liir.nlp.srl.sources.lth.interfaces.LundSRL;
import org.xml.sax.SAXException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Created by quynhdo on 01/09/15.
 */
public class Test {

    public static void main (String[] args) throws IOException, ClassNotFoundException, SAXException {
        String txt = new String(Files.readAllBytes(Paths.get("/Users/quynhdo/Documents/WORKING/MUSE/data/HASTexts/PG.txt")));
       // txt= "Mary and Peter are going to school. They like to be there.";

        BerSentenceSplitter bs = new BerSentenceSplitter("../Coref/models/sentsplit.txt.gz");
        BerParser bp = new BerParser("../Coref/models/eng_sm6.gr","../Coref/models/eng_sm1.gr");
        Text text = bs.processWithTokenizer(txt);


        System.out.println(text.toXMLString());

        bp.processToXML(text);
        System.out.println(text.toXMLString());

        text.setAutomaticIndexing();
        LundParser lp = new LundParser("/Users/quynhdo/Downloads/CoNLL2009-ST-English-ALL.anna-3.3.parser.model");
        LundSRL srl = new LundSRL("/Users/quynhdo/Downloads/CoNLL2009-ST-English-ALL.anna-3.3.srl-4.1.srl.model");

        String txtt = lp.process(text.toXMLString());

        Text text1 = XMLReader.readCorpus(srl.process(txtt)).get(0);

        bp.processToXML(text1);
        System.out.println(text1.toXMLString());

        DriverExtended de = new DriverExtended("../Coref/models/coref-onto.ser.gz", "../Coref/data/gender.data");
        de.runPrediction(text1);
        System.out.println(text1.toXMLString());


        Files.write(Paths.get("/Users/quynhdo/Desktop/PG.xml"), text1.toXMLString().getBytes());



    }
}
