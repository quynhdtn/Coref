package liir.nlp.sources.bekerley.coref;

import edu.berkeley.nlp.entity.Driver;
import edu.berkeley.nlp.entity.GUtil;
import edu.berkeley.nlp.entity.coref.*;
import edu.berkeley.nlp.util.Logger;
import liir.nlp.core.representation.Text;
import liir.nlp.core.representation.entities.*;
import liir.nlp.core.representation.entities.Mention;
import scala.Tuple3;
import scala.Tuple2;

import scala.collection.Seq;
import scala.collection.JavaConversions;

import java.util.ArrayList;
import java.util.HashMap;


/**
 * Created by quynhdo on 01/09/15.
 */
public class DriverExtended extends Driver {

    PairwiseScorer scorer;

    public DriverExtended(String modelPath, String genderPath){

        numberGenderDataPath = genderPath;

        scorer =  (PairwiseScorer) GUtil.load(modelPath);
    }

    public Seq<Tuple2<DocumentGraph, float[][]>> runFullPrediction(String test, String model){
        Logger.setFig();
        return CorefSystemExtended.runExtractionOutput(test, -1, model);
    }


    public void runPrediction(Text text){
        Logger.setFig();

        Tuple3<int[], OrderedClustering, DocumentGraph> rs=   CorefSystemExtended.runExtractionBody(Conversion.convertTextToConllDoc(text), scorer);

        java.util.List<edu.berkeley.nlp.entity.coref.Mention> mss =JavaConversions.bufferAsJavaList(rs._3().corefDoc().predMentions().toBuffer());

        HashMap<String,String> idMapping =new HashMap<>(); // id in coref - id indexed in text

        for (edu.berkeley.nlp.entity.coref.Mention m : mss){

            liir.nlp.core.representation.entities.Mention  mym = new Mention(String.valueOf(m.startIdx()+1),
                    String.valueOf(m.endIdx()), String.valueOf(m.headIdx()+1), String.valueOf(m.sentIdx()));
            mym.setId(String.valueOf(m.mentIdx()));
            String mapId = text.addMentionWithIndexing(mym);
            idMapping.put(String.valueOf(m.mentIdx()), mapId);

        }

        OrderedClustering oc  =rs._2();

        java.util.List<scala.collection.mutable.ArrayBuffer<Integer>> occ =   JavaConversions.bufferAsJavaList(oc.clusters().toBuffer());


        for (int i =0;i<occ.size(); i++){
            java.util.List<Integer> jcl = JavaConversions.bufferAsJavaList(occ.get(i));
            ArrayList<String> cll = new ArrayList<>();
            for (int idd : jcl){
                cll.add(idMapping.get(String.valueOf(idd)));
            }
            text.addCorefCluster(new MentionCluster(cll));


        }


        System.out.print("");
    }


    public static void runPredictionWithILP(Text text, DocumentGraph doc, OrderedClustering oc) {
        Logger.setFig();


        java.util.List<edu.berkeley.nlp.entity.coref.Mention> mss = JavaConversions.bufferAsJavaList(doc.corefDoc().predMentions().toBuffer());

        HashMap<String, String> idMapping = new HashMap<>(); // id in coref - id indexed in text

        for (edu.berkeley.nlp.entity.coref.Mention m : mss) {

            liir.nlp.core.representation.entities.Mention mym = new Mention(String.valueOf(m.startIdx() + 1),
                    String.valueOf(m.endIdx()), String.valueOf(m.headIdx() + 1), String.valueOf(m.sentIdx()));
            mym.setId(String.valueOf(m.mentIdx()));
            String mapId = text.addMentionWithIndexing(mym);
            idMapping.put(String.valueOf(m.mentIdx()), mapId);

        }



        java.util.List<scala.collection.mutable.ArrayBuffer<Integer>> occ = JavaConversions.bufferAsJavaList(oc.clusters().toBuffer());


        for (int i = 0; i < occ.size(); i++) {
            java.util.List<Integer> jcl = JavaConversions.bufferAsJavaList(occ.get(i));
            ArrayList<String> cll = new ArrayList<>();
            for (int idd : jcl) {
                cll.add(idMapping.get(String.valueOf(idd)));
            }
            text.addCorefCluster(new MentionCluster(cll));


        }


        System.out.print("");
    }

    public Tuple2<DocumentGraph,float[][]> getScores(Text text){
        Logger.setFig();

        Tuple2<DocumentGraph,float[][]> rs=   CorefSystemExtended.runExtractionOutputBody(Conversion.convertTextToConllDoc(text), scorer);

        return rs;

    }
}
