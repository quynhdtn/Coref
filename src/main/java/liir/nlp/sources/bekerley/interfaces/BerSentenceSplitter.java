package liir.nlp.sources.bekerley.interfaces;

import edu.berkeley.nlp.entity.preprocess.SentenceSplitter;
import liir.nlp.representation.Sentence;
import liir.nlp.representation.Text;
import liir.nlp.representation.Word;

/**
 * Created by quynhdo on 01/09/15.
 */
public class BerSentenceSplitter {
    SentenceSplitter splitter;
    public static boolean respectInputLineBreaks = true;
    public static boolean respectInputTwoLineBreaks = true;

    public BerSentenceSplitter(String sentenceSplitterModelPath) {
      splitter =  SentenceSplitter.loadSentenceSplitter(sentenceSplitterModelPath);

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

        String[][] tokenizedSentences = splitter.tokenize(sentences);
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
