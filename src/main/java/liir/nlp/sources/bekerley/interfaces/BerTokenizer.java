package liir.nlp.sources.bekerley.interfaces;

import edu.berkeley.nlp.entity.preprocess.SentenceSplitter;
import liir.nlp.interfaces.preprocessing.Tokenizer;
import liir.nlp.representation.Sentence;
import liir.nlp.representation.Text;
import liir.nlp.representation.Word;

/**
 * Created by quynhdo on 01/09/15.
 */
public class BerTokenizer extends Tokenizer{
    SentenceSplitter splitter;


    public BerTokenizer(String sentenceSplitterModelPath) {
        splitter =  SentenceSplitter.loadSentenceSplitter(sentenceSplitterModelPath);
        setName("Bekerley Tokenizer");

    }

    public Text processToXML(String[] sentences){
        String[][] tokenizedSentences = SentenceSplitter.tokenize(sentences);
        Text txt = new Text();
        for (int i =0 ; i<tokenizedSentences.length; i++) {
            Sentence se =new Sentence();
            for (int j = 0; j < tokenizedSentences[i].length; j++)
            {
                Word w = new Word();
                w.setStr(tokenizedSentences[i][j]);
                se.add(w);
            }
            txt.add(se);

        }

        return txt;
    }


    public String[] process(String str){

       return  SentenceSplitter.tokenizeSingle(str);
    }


}
