package liir.nlp.sources.bekerley.coref

import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.coref._
import edu.berkeley.nlp.futile.util.Logger

/**
 * Created by quynhdo on 01/09/15.
 */
object CorefSystemExtended {


   def runExtractionOutput(devPath: String, devSize: Int, modelPath: String):Seq[(DocumentGraph,Array[Array[Float]])]= {
     runExtractionOutputBody(devPath, devSize, GUtil.load(modelPath).asInstanceOf[PairwiseScorer]);
   }

  def runExtractionOutput(doc: ConllDoc,  modelPath: String):(DocumentGraph,Array[Array[Float]])= {
    runExtractionOutputBody(doc,  GUtil.load(modelPath).asInstanceOf[PairwiseScorer]);
  }


  def runExtraction(doc: ConllDoc,  modelPath: String):(Array[Int],OrderedClustering,DocumentGraph)= {
    runExtractionBody(doc,  GUtil.load(modelPath).asInstanceOf[PairwiseScorer]);
  }

   def runExtractionOutputBody(doc: ConllDoc,  scorer: PairwiseScorer) : (DocumentGraph,Array[Array[Float]])= {
     // Read because it should be a directory
     val devDocGraph = prepareTestDocuments(doc);
     //    new CorefFeaturizerTrainer().featurizeBasic(devDocGraphs, scorer.featurizer);  // dev docs already know they are dev docs so they don't add features
     //    val basicInferencer = new DocumentInferencerBasic();
     runPredictForMe(devDocGraph, scorer, false);


   }

  def runExtractionBody(doc: ConllDoc,  scorer: PairwiseScorer) : (Array[Int],OrderedClustering, DocumentGraph) = {
    // Read because it should be a directory
    val devDocGraph = prepareTestDocuments(doc);
    //    new CorefFeaturizerTrainer().featurizeBasic(devDocGraphs, scorer.featurizer);  // dev docs already know they are dev docs so they don't add features
    //    val basicInferencer = new DocumentInferencerBasic();
    runPredict(devDocGraph, scorer);


  }

  def runExtractionOutputBody(devPath: String, devSize: Int, scorer: PairwiseScorer) : Seq[(DocumentGraph,Array[Array[Float]])]= {
    // Read because it should be a directory
    val devDocGraphs = CorefSystem.prepareTestDocuments(devPath, devSize);
    //    new CorefFeaturizerTrainer().featurizeBasic(devDocGraphs, scorer.featurizer);  // dev docs already know they are dev docs so they don't add features
    //    val basicInferencer = new DocumentInferencerBasic();
    runPredictForMe(devDocGraphs, scorer, false);


  }

  def loadCorefDocs(doc:ConllDoc, maybeNumberGenderComputer: Option[NumberGenderComputer]): CorefDoc = {
    val assembler = CorefDocAssembler(Driver.lang, Driver.useGoldMentions);
    val mentionPropertyComputer = new MentionPropertyComputer(maybeNumberGenderComputer);
    if (Driver.useCoordination) {
     val corefDoc = assembler.createCorefDocWithCoordination(doc, mentionPropertyComputer);
      return corefDoc
    } else {
      val corefDoc = assembler.createCorefDoc(doc, mentionPropertyComputer);
      return  corefDoc
    }

  }

  def preprocessDocsCacheResources(docGraph: DocumentGraph) {
    if (Driver.wordNetPath != "") {
      val wni = new WordNetInterfacer(Driver.wordNetPath);
      docGraph.cacheWordNetInterfacer(wni);
    }
  }


  def prepareTestDocuments(devDoc: ConllDoc): DocumentGraph = {
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData( Driver.numberGenderDataPath);
    val corefDoc = loadCorefDocs(devDoc, Some(numberGenderComputer));

    val devDocGraph = new DocumentGraph(corefDoc, false);
    preprocessDocsCacheResources(devDocGraph);
    CorefPruner.buildPruner(Driver.pruningStrategy).prune(devDocGraph);
    devDocGraph;
  }

   /**
    * run prediction return a document graph with the full array of scores
    * @param devDocGraphs
    * @param scorer
    * @param isParallel
    * @return
    */
   def runPredictForMe(devDocGraphs: Seq[DocumentGraph], scorer: PairwiseScorer, isParallel: Boolean): Seq[(DocumentGraph,Array[Array[Float]])] = {
     val basicInferencer = new DocumentInferencerBasic();
     val indices = (0 until devDocGraphs.size);
     Logger.startTrack("Decoding dev");
     val results = (if (isParallel) indices.par else indices).map(i => {
       Logger.logss("Decoding " + i);
       val devDocGraph = devDocGraphs(i);
       devDocGraph.featurizeIndexNonPrunedUseCache(scorer.featurizer);


       val (featsChart, scoresChart) = devDocGraph.featurizeIndexAndScoreNonPrunedUseCache(scorer);

       val probFcn = (idx: Int) => {
         val probs = scoresChart(idx);
         GUtil.expAndNormalizeiHard(probs);
         probs;
       }

       DocumentInferencerBasic.decodeMax(scoresChart.size, probFcn);
       val (backptrs, clustering) = basicInferencer.viterbiDecodeFormClustering(devDocGraph, scorer);
       //  devDocGraph.clearFeatureCache();
       (devDocGraph, scoresChart);
     }).toIndexedSeq;
     Logger.endTrack();
     results;
   }


  def runPredictForMe(devDocGraph: DocumentGraph, scorer: PairwiseScorer, isParallel: Boolean): (DocumentGraph,Array[Array[Float]]) = {
    val basicInferencer = new DocumentInferencerBasic();

      devDocGraph.featurizeIndexNonPrunedUseCache(scorer.featurizer);


      val (featsChart, scoresChart) = devDocGraph.featurizeIndexAndScoreNonPrunedUseCache(scorer);

      val probFcn = (idx: Int) => {
        val probs = scoresChart(idx);
        GUtil.expAndNormalizeiHard(probs);
        probs;
      }

      DocumentInferencerBasic.decodeMax(scoresChart.size, probFcn);
      val (backptrs, clustering) = basicInferencer.viterbiDecodeFormClustering(devDocGraph, scorer);
        devDocGraph.clearFeatureCache();
      (devDocGraph, scoresChart);

  }

  def runPredict(devDocGraph: DocumentGraph, scorer: PairwiseScorer): (Array[Int],OrderedClustering,DocumentGraph) = {
    val basicInferencer = new DocumentInferencerBasic();

    devDocGraph.featurizeIndexNonPrunedUseCache(scorer.featurizer);


    val (featsChart, scoresChart) = devDocGraph.featurizeIndexAndScoreNonPrunedUseCache(scorer);

    val probFcn = (idx: Int) => {
      val probs = scoresChart(idx);
      GUtil.expAndNormalizeiHard(probs);
      probs;
    }

    DocumentInferencerBasic.decodeMax(scoresChart.size, probFcn);
    val (backptrs, clustering) = basicInferencer.viterbiDecodeFormClustering(devDocGraph, scorer);
      devDocGraph.clearFeatureCache();
    (backptrs, clustering, devDocGraph);
  }
 }
