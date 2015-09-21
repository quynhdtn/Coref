package liir.nlp.sources.bekerley.interfaces;

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser;
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver;
import edu.berkeley.nlp.syntax.Tree;
import edu.berkeley.nlp.util.Logger;
import liir.nlp.interfaces.preprocessing.Processor;
import liir.nlp.representation.Sentence;
import liir.nlp.representation.Text;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by quynhdo on 01/09/15.
 * this parser does both syntactic parsing and tokenizer
 *
 */
public class BerParser extends Processor{
    CoarseToFineMaxRuleParser parser;
    CoarseToFineMaxRuleParser backoffParser;

    public BerParser(String grammarModelPath, String backoffgrammarModelPath){

        super("Bekerley Parser");
        parser = PreprocessingDriver.loadParser(grammarModelPath);
        backoffParser = PreprocessingDriver.loadParser(backoffgrammarModelPath);


    }


    public Text processToText(Text txt){

        for (Sentence s : txt) {
            ArrayList<String> forms = new ArrayList<String>();
            s.forEach(w -> forms.add(w.getStr()));
            Tree<String> parse = PreprocessingDriver.parse(parser, backoffParser, forms);
            String[] parsebits = PreprocessingDriver.computeParseBits(parse);



            if (parse.getYield().size() != s.size()) {
                Logger.logss("WARNING: couldn't parse sentence, dropping it! " );
                Logger.logss("  (This will be fixed to backing off to an X-bar grammar in a future release)");
            } else {
                String[] posTags = new String[s.size()];
                List<String> preterminals = parse.getPreTerminalYield();
                for (int i = 0; i < preterminals.size(); i++) {
                    posTags[i] = preterminals.get(i);


                }


                for (int i = 0; i < s.size(); i++) {
                    s.get(i).addFeature("parseBit", parsebits[i]);
                    s.get(i).setPos(posTags[i]);

                }

            }


        }

        return txt;

    }

}
