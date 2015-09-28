package liir.nlp.sources.bekerley.interfaces;

import edu.berkeley.nlp.entity.preprocess.SentenceSplitter;
import liir.nlp.core.representation.Sentence;
import liir.nlp.core.representation.Text;
import liir.nlp.core.representation.Word;

/**
 * Created by quynhdo on 01/09/15.
 */
public class BerSentenceSplitter  extends liir.nlp.interfaces.preprocessing.SentenceSplitter{
    SentenceSplitter splitter;
    public static boolean respectInputLineBreaks = true;
    public static boolean respectInputTwoLineBreaks = true;

        public BerSentenceSplitter(String sentenceSplitterModelPath) {
      splitter =  SentenceSplitter.loadSentenceSplitter(sentenceSplitterModelPath);
        setName("Bekerley Sentence Splitter");
    }





    public String[] process (String text){
        String[] lines = text.split("\n");

        String[] canonicalizedParagraphs = splitter.formCanonicalizedParagraphs(lines, respectInputLineBreaks, respectInputTwoLineBreaks);

        String[] sentences = splitter.splitSentences(canonicalizedParagraphs);

        return sentences;

    }

    public Text processWithTokenizer(String text){
        String[] lines = text.split("\n");

        String[] canonicalizedParagraphs = splitter.formCanonicalizedParagraphs(lines, respectInputLineBreaks, respectInputTwoLineBreaks);

        String[] sentences = splitter.splitSentences(canonicalizedParagraphs);

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



}
