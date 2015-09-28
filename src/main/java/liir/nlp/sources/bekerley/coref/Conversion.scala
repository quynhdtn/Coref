package liir.nlp.sources.bekerley.coref

import edu.berkeley.nlp.entity.lang.ModCollinsHeadFinder
import edu.berkeley.nlp.entity.{ConllDoc, DepConstTree, ConllDocReader}
import liir.nlp.core.representation.Text

import scala.collection.mutable.ArrayBuffer

/**
 * Created by quynhdo on 01/09/15.
 */
object Conversion {

  def makeArray(wordss: ArrayBuffer[ArrayBuffer[String]], symbol: String): ArrayBuffer[ArrayBuffer[String]] = {
    var s = symbol
    var buff = ArrayBuffer[ArrayBuffer[String]]();
    for (i <- 0 until wordss.size) {
      var buffs = ArrayBuffer[String]()
      for (w <- 0 until wordss(i).size)
        buffs :+= s

      buff:+=buffs
    }
    buff
   }

  def readText(txt: Text): (ArrayBuffer[ArrayBuffer[String]],ArrayBuffer[ArrayBuffer[String]],ArrayBuffer[ArrayBuffer[String]], String) = {
    var wordss = ArrayBuffer[ArrayBuffer[String]]()
    var poss = ArrayBuffer[ArrayBuffer[String]]()
    var parseBitss = ArrayBuffer[ArrayBuffer[String]]()

    for (i <- 0 until txt.size()) {

      var wordssSe = ArrayBuffer[String]()
      var possSe = ArrayBuffer[String]()
      var parseBitssSe = ArrayBuffer[String]()

      for (j <- 0 until txt.get(i).size()) {

        wordssSe :+= txt.get(i).get(j).getStr
        possSe :+= txt.get(i).get(j).getPos
        parseBitssSe :+= txt.get(i).get(j).getFeature("parseBit")


      }

      wordss :+=wordssSe
      poss :+=possSe
      parseBitss :+=parseBitssSe


    }

    (wordss,poss,parseBitss, txt.getId)

  }

  def convertTextToConllDoc(txt: Text): ConllDoc ={
   // val docFields = docBySentencesByLinesFixed.map(_.map(_.split("\\s+")));

    val mytxt = readText(txt)
    val wordss = mytxt._1
    val poss = mytxt._2
   val parseBitss = mytxt._3
    val speakerss = makeArray(wordss, "_")
    val nerBitss = makeArray(wordss, "*")
    val corefBitss = makeArray(wordss, "-")

    val headFinder = new ModCollinsHeadFinder();

    val trees = for (i <- 0 until wordss.size) yield {
      val constTree =
        ConllDocReader.assembleConstTree(wordss(i), poss(i), parseBitss(i))

      val childParentMap = DepConstTree.extractDependencyStructure(constTree, headFinder);
      new DepConstTree(constTree, poss(i), wordss(i), childParentMap)
    }

    new ConllDoc(mytxt._4,
      0,
      wordss,
      poss,
      trees,
      nerBitss.map(ConllDocReader.assembleNerChunks(_)),
      corefBitss.map(ConllDocReader.assembleCorefChunks(_)),
      speakerss)



  }

}
