package liir.nlp.coref.ilp


import liir.nlp.coref.ilp.ILPModel
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import liir.nlp.coref.representation._
import edu.berkeley.nlp.entity.coref.MentionType

import liir.nlp.coref.discourse.NewQuotationHandler
import scala.io.Source._

class MyILP(window: Int, config: String) {
        var  m : ILPModel = new ILPModel();
        var use_basic = true
        var use_disjoint = true
        var use_protop = true
        var use_famdef = true
        var use_align = true
        var use_quote = true
        var use_exact = true
        var PENALTIES=readConfig(config)
        val reflexive_prons = Set("myself", "himself", "herself", "itself", "themself", "ourself")
        var vars = new java.util.ArrayList[String]()  //ILP variables
        var weights= new java.util.ArrayList[java.lang.Double]() // weight of ILP variables
        
        
  
      def readProblem (score_chart: Array[Array[Float]], txt: Text): (Array[Int],Array[Int])={
        
          
          
          setupFramework(score_chart, txt)
          if (use_basic){
            println ( "processing basic constraint...")
            computeBasicConstraint(score_chart, txt)
          }
          if (use_disjoint){
              println ( "processing disjoint constraint...")

            computeDisjointConstraint(txt)
          }
          
            if (use_protop){
              println ( "processing protop constraint...")

            computeProtopConstraint(txt)
          }
        
           if (use_famdef){
              println ( "processing famdef constraint...")

            computeFamdefConstraint(txt)
          }
        
            if (use_align){
              println ( "processing align constraint...")

            computeAlignConstraint(txt)
          }
            
            if (use_quote){
               println ( "processing align constraint...")

              computeQuotationConstraint(txt)
            }
            
             if (use_exact){
               println ( "processing exact constraint...")

              computeExactConstraint(score_chart,txt)
            }
          m.setObjective(vars, weights, 1);

           m.writeModel("model.test.lp")
 //           m.optimizeAndSave("out.txt")
            
         m.optimize_only()
     
         var rs = m.get_results()
         var backpts = computeBackPtsNormalSolution(score_chart)
         var new_backpts = backpts.clone()
        
          var entries = rs.keySet().iterator()
             while (entries.hasNext()){
                  var e = entries.next()
                   if (rs.get(e) ==1.0)
                     { 
                       var tmps = e.split("\\_")
                       if (tmps(0).equals("u")){
                         new_backpts.update(tmps(1).toInt, tmps(2).toInt)
                       }
                  
                     }
             }
                 
       (backpts,new_backpts)
   }
        
  def readConfig(config: String): Array[Float]={
    
     val lines = fromFile(config,  "utf-8").getLines.toList 
     var basic_p = -100F
     var disjoint_p = -0.3F
     var protop_p = -0.3F
     var famdef_p = -0.2F
     var align_p = -0.1F
     var quote_p = -1.0F
     var exact_p = -100F

     for (l <- lines){
       val pts = l.split("\\s+")
       if (pts.length > 0){
         if (pts(0) == "use_basic"){
           if (pts (1) == "true") {
             basic_p = pts(2).toFloat
           }
           else
             use_basic = false
         }
         
          if (pts(0) == "use_disjoint"){
           if (pts (1) == "true") {
             disjoint_p = pts(2).toFloat
           }
           else
             use_disjoint = false
         }
          
          if (pts(0) == "use_protop"){
           if (pts (1) == "true") {
             protop_p = pts(2).toFloat
           }
           else
             use_protop = false
         }
          
          if (pts(0) == "use_famdef"){
           if (pts (1) == "true") {
             famdef_p = pts(2).toFloat
           }
           else
             use_famdef = false
         }
         
         if (pts(0) == "use_align"){
           if (pts (1) == "true") {
             align_p = pts(2).toFloat
           }
           else
             use_align = false
         }
         
         
         
          if (pts(0) == "use_quote"){
           if (pts (1) == "true") {
             quote_p = pts(2).toFloat
           }
           else
             use_quote = false
         }
          
           if (pts(0) == "use_exact"){
           if (pts (1) == "true") {
             exact_p = pts(2).toFloat
           }
           else
             use_exact = false
         }
         
       }
     }
     
     return Array[Float](basic_p, disjoint_p, protop_p, famdef_p, align_p, quote_p, exact_p)
  }
  def setupFramework(score_chart: Array[Array[Float]], txt: Text ) {
     for (i <- 0 until score_chart.length)  // for mention m_i
        for (j <-0  until score_chart(i).length) // for mention m_j
          {
            m.addBinaryVar("u"+ "_" + i.toString() +"_"+ j.toString())      
            vars.add("u"+ "_" + i.toString() +"_"+ j.toString())
            weights.add ((score_chart(i)(j)).toDouble)
      
        }
        
         for (i <- 0 until score_chart.length) 
          {
            var varstmp = new java.util.ArrayList[String]()
            var weightstmp= new java.util.ArrayList[java.lang.Double]() 
            for (j <-0  until score_chart(i).length)
            {

            varstmp.add("u"+ "_" + i.toString() +"_"+ j.toString())


              weightstmp.add(1.0)
            }
            m.addNormalConstraint(varstmp, weightstmp, "==", 1)
          }
         
         
         for (i<-0 until score_chart.length){
           
           for (j<- 0 until i){
         //    if (j>=0)
             m.addBinaryVar("v"+ "_" + i.toString() +"_"+ j.toString()) 
            
           }
         }
         
           for (i<-0 until score_chart.length){

           for (j<- i-window until i){
             if (j>=0){
              var varstmp = new java.util.ArrayList[String]()
              var weightstmp= new java.util.ArrayList[java.lang.Double]() 
              varstmp.add("u"+ "_" + i.toString() +"_"+ i.toString())
            
              varstmp.add("v"+ "_" + i.toString() +"_"+ j.toString())
              weightstmp.add (1.0)
              weightstmp.add (1.0)
             
              m.addNormalConstraint(varstmp, weightstmp, "<=", 1)
             }
                          
           }
           
           }
           
           for (i<-0 until score_chart.length){

           
           for (j<- i-window until i){
             if (j>=0){
               var varstmp = new java.util.ArrayList[String]()
              var weightstmp= new java.util.ArrayList[java.lang.Double]() 
              varstmp.add("u"+ "_" + i.toString() +"_"+ j.toString())
            
              varstmp.add("v"+ "_" + i.toString() +"_"+ j.toString())
              weightstmp.add (1.0)
              weightstmp.add (-1.0)
             
              m.addNormalConstraint(varstmp, weightstmp, "<=", 0)
             }
           }
           }
               
           
           
           for (i<-0 until score_chart.length){

           
           for (j<- i-window until i){
             
             for (k<- i-window until j){
               if (k>=0){
               
                var varstmp = new java.util.ArrayList[String]()
                var weightstmp= new java.util.ArrayList[java.lang.Double]() 
                varstmp.add("u"+ "_" + i.toString() +"_"+ j.toString())
                varstmp.add("v"+ "_" + j.toString() +"_"+ k.toString())
                varstmp.add("v"+ "_" + i.toString() +"_"+ k.toString())
                weightstmp.add (1.0)
                weightstmp.add (1.0)
                weightstmp.add (-1.0)
               
                m.addNormalConstraint(varstmp, weightstmp, "<=", 1)
                
                weightstmp.clear()
                weightstmp.add (1.0)
                weightstmp.add (-1.0)
                weightstmp.add (1.0)
                m.addNormalConstraint(varstmp, weightstmp, "<=", 1)
              
                weightstmp.clear()
               }
                
             }
             
           }
          
           }
           
           
           /// calculate v
           for (i<-0 until score_chart.length){
           for (j<- i-window until i){

               if (j>=0){
              for (k<- j+1 until i){
               
                var varstmp = new java.util.ArrayList[String]()
                var weightstmp= new java.util.ArrayList[java.lang.Double]() 
                varstmp.add("u"+ "_" + i.toString() +"_"+ j.toString())
                varstmp.add("v"+ "_" + k.toString() +"_"+ j.toString())
                varstmp.add("v"+ "_" + i.toString() +"_"+ k.toString())
                weightstmp.add (1.0)
                weightstmp.add (1.0)
                weightstmp.add (-1.0)
            //    println(i.toString + "| " + j.toString + "| " + k.toString)
                m.addNormalConstraint(varstmp, weightstmp, "<=", 1)
                
                weightstmp.clear()
                weightstmp.add (1.0)
                weightstmp.add (-1.0)
                weightstmp.add (1.0)
                m.addNormalConstraint(varstmp, weightstmp, "<=", 1)
              
                }
             }
                          
           }
           
           
           
           
           
         }
           

  }  
  
  
  def computeBasicConstraint(score_chart: Array[Array[Float]], txt: Text)  {
    
    // link definite nominal mention to its first occurence.
      for  (i<-1 until score_chart.length) // for mention m_i
        for (j <- i-window  until score_chart(i).length-1)
        {
          
          if (j>=0){
                        if ((txt.predicted_mentions(i).mentionType != MentionType.PRONOMINAL) && (txt.predicted_mentions(i).mentionType != MentionType.DEMONSTRATIVE))
                        {
          var x1= txt.predicted_mentions(i).words.filter { w => txt.predicted_mentions(i).words.indexOf(w) >=1 }.toArray.deep.mkString(" ").toLowerCase
           var x2= txt.predicted_mentions(i).words.filter { w => txt.predicted_mentions(j).words.indexOf(w) >=1 }.toArray.deep.mkString(" ").toLowerCase
         
          if (x1 == x2)
            if (txt.predicted_mentions(i).words(0).toLowerCase == txt.predicted_mentions(j).words(0).toLowerCase || 
            ( txt.predicted_mentions(i).words(0).toLowerCase == "the") && (txt.predicted_mentions(j).words(0).toLowerCase == "a" ) || 
            ( txt.predicted_mentions(i).words(0).toLowerCase == "the") && (txt.predicted_mentions(j).words(0).toLowerCase == "an" ) || 
            ( txt.predicted_mentions(i).words(0).toLowerCase == "an") && (txt.predicted_mentions(j).words(0).toLowerCase == "the" ) || 
            ( txt.predicted_mentions(i).words(0).toLowerCase == "a") && (txt.predicted_mentions(j).words(0).toLowerCase == "the" ) )
           // txt.predicted_mentions(i).words(0).toLowerCase == txt.predicted_mentions(j).words(0).toLowerCase  )
            {
               m.addBinaryVar("nc"+ "_" + i.toString() +"_"+ j.toString())
                  vars.add("nc"+ "_" + i.toString() +"_"+ j.toString())
              
                  weights.add (PENALTIES(0).toDouble)  //penalty over mentions
                  var varstmp = new java.util.ArrayList[String]()
                  var weightstmp= new java.util.ArrayList[java.lang.Double]()
                  varstmp.add("v"+ "_" + i.toString() +"_"+ j.toString())
                  varstmp.add("nc"+ "_" + i.toString() +"_"+ j.toString())
                  weightstmp.add(1.0)
                  weightstmp.add(1.0)
                  m.addNormalConstraint(varstmp, weightstmp, "==", 1)
         
              
            }
                        
                        }
              
          }
        }
  }
        
  def computeDisjointConstraint(txt: Text) = {
           var disjoints = Array[Tuple2[Int,Int]] ()
        for (se <- txt.sentences){
        //     var predicates = se.const_predicates
             var all_mentions = se.getMentionIds   //all mentions
       
             for (m1 <- 1 until all_mentions.length){
               
               if (! (reflexive_prons contains txt.predicted_mentions(all_mentions(m1)).words.toArray.deep.mkString(" ").toLowerCase )){
                 
            //     println (txt.predicted_mentions(all_mentions(m1)).words.toArray.deep.mkString(" ").toLowerCase )
               for (m2 <- 0 until m1){
                 
             //    if (txt.predicted_mentions(all_mentions(m1)).mentionType == MentionType.PRONOMINAL){
             //     if ( se.findCoFrames(txt.predicted_mentions(all_mentions(m2)).startIdx, txt.predicted_mentions(all_mentions(m2)).endIdx, txt.predicted_mentions(all_mentions(m1)).startIdx, txt.predicted_mentions(all_mentions(m1)).endIdx).length   >= 0)
       
                 
                     if ( se.findCoFramesSubjectObject(txt.predicted_mentions(all_mentions(m2)).startIdx, txt.predicted_mentions(all_mentions(m2)).endIdx, txt.predicted_mentions(all_mentions(m1)).startIdx, txt.predicted_mentions(all_mentions(m1)).endIdx)  >= 0)
                       disjoints:+=(all_mentions(m2), all_mentions(m1))
                       
            //     }                        
                 }
              
               }
           
             }
        //   }
         }
       
           
     for (mp <- disjoints)
      {
        
        // for each mp, add one new variable dc_mp2_mp1
        //if u_mp2_mp1 == 1, then dc_mp2_mp1=1, else dc_mp2_mp1=0
                  m.addBinaryVar("dc"+ "_" + mp._2.toString() +"_"+ mp._1.toString())
                  vars.add("dc"+ "_" + mp._2.toString() +"_"+ mp._1.toString())
              
           //       weights.add (PENALTIES(0)*(mp._2+1).toDouble)  //penalty over mentions
                  weights.add (PENALTIES(1).toDouble)  //penalty over mentions

                  var varstmp = new java.util.ArrayList[String]()
                  var weightstmp= new java.util.ArrayList[java.lang.Double]()
                  varstmp.add("v"+ "_" + mp._2.toString() +"_"+ mp._1.toString())
                  varstmp.add("dc"+ "_" + mp._2.toString() +"_"+ mp._1.toString())
                  weightstmp.add(1.0)
                  weightstmp.add(-1.0)
                  m.addNormalConstraint(varstmp, weightstmp, "<=", 0)
        
                  
        
      }
     
   }
  
  
  
   
   
   
    def computeProtopConstraint(txt: Text)  {
     
     var protops = Array[(Int, Array[(Int, Int)])]()
     for (i <- 1 until txt.length){
       var rs = Array[(Int, Int)]()
       val prons = txt.sentences(i).getPronMentionIds
       
       val cans = txt.sentences.filter { s => txt.sentences.indexOf(s) >= i-3 && txt.sentences.indexOf(s) < i }.flatMap { s => s.getMentionIds }
       for (p<-prons)
          {
            for (c<-cans)
              rs :+= (c, p)
          }
      if (rs.length >0)
       protops :+= (i, rs)
     }
     
     for (protop <- protops){
       
                  m.addBinaryVar("pt"+ "_" + protop._1.toString())
                  vars.add("pt"+ "_" +  protop._1.toString())
              
                  weights.add (PENALTIES(2).toDouble)  //penalty over mentions
                   var varstmp1 = new java.util.ArrayList[String]()
                   var weightstmp1 = new java.util.ArrayList[java.lang.Double]()
                  for (p <- protop._2){
                    
                    if (p._1 >= p._2 - window){
                        var varstmp = new java.util.ArrayList[String]()
                        var weightstmp= new java.util.ArrayList[java.lang.Double]()
                        varstmp.add("v"+ "_" + p._2.toString() +"_"+ p._1.toString())
                        varstmp.add("pt"+ "_" + protop._1.toString())
                        weightstmp.add(1.0)
                        weightstmp.add(1.0)
                        
                        varstmp1.add("v"+ "_" + p._2.toString() +"_"+ p._1.toString())
                        weightstmp1.add(1.0)
                       m.addNormalConstraint(varstmp, weightstmp, "<=", 1)
                        
                    }
                  
                  }
                  
                  varstmp1.add("pt"+ "_" + protop._1.toString())
                  weightstmp1.add(1.0)
        
                  m.addNormalConstraint(varstmp1, weightstmp1, ">=", 1)

     }
     
     
     
   }
    
    def computeAlignConstraint(txt: Text) {
      var rs = Array[(Int,Int)]()
      for (i <- 1 until txt.length){
        if (txt.sentences(i).words.filter { w => Set("\"", "``", "''" , "'", "`") contains w.form }.length ==0 ){
   
        val prons = txt.sentences(i).getPronMentionIds
        var select_men = -1
        if (prons.length == 1){
          val prev_sen= txt.sentences(i-1)
          if (prev_sen.const_predicates.length ==1)
          {
            val prev_mens = prev_sen.getMentionIds
            for (m<- prev_mens){
            val lbl =   prev_sen.getSemanticLabel(0, txt.predicted_mentions(m).startIdx, txt.predicted_mentions(m).endIdx)
           
            if (lbl == "A0"){
              select_men = m
            }
            }
            
        }
        
        if (select_men != -1)
          rs :+= (select_men, prons(0))
        
            }
          }
        }
     for (al <- rs){
       if (al._1 >= al._2-window){
         m.addBinaryVar("al"+ "_" +al._2.toString())
                  vars.add("al"+ "_" +  al._2.toString())
              
                  weights.add (PENALTIES(4).toDouble)  //penalty over mentions
                  
                  var varstmp = new java.util.ArrayList[String]()
                        var weightstmp= new java.util.ArrayList[java.lang.Double]()
                        varstmp.add("u"+ "_" + al._2.toString() +"_"+ al._1.toString())
                        varstmp.add("al"+ "_" + al._2.toString())
                        weightstmp.add(1.0)
                        weightstmp.add(1.0)
                        
                       m.addNormalConstraint(varstmp, weightstmp, "==", 1)
                       
                       
       }
        
     }
    }
   
    def computeFamdefConstraint(txt:Text) {
      
      var famdefs = sentenceWithoutQuote(txt).flatMap { s => s.getPronMentionIds() }
      
      for (i<- famdefs){
                  m.addBinaryVar("fd"+ "_" +i.toString())
                  vars.add("fd"+ "_" +  i.toString())
              
                  weights.add (PENALTIES(3).toDouble)  //penalty over mentions
                  
                  var varstmp = new java.util.ArrayList[String]()
                        var weightstmp= new java.util.ArrayList[java.lang.Double]()
                        varstmp.add("u"+ "_" + i.toString() +"_"+ i.toString())
                        varstmp.add("fd"+ "_" + i.toString())
                        weightstmp.add(1.0)
                        weightstmp.add(-1.0)
                        
                       m.addNormalConstraint(varstmp, weightstmp, "==", 0)
        
      }
    
    
    
    }
    
    
    def computeQuotationConstraint(txt: Text){
       
      val qh = new NewQuotationHandler()
      val quotess = qh.findQuotation(txt)
      
      for (quotes <- quotess){
        
        print(quotes.open_quote)
        print("\t")
        print(quotes.close_quote)
        print(": ")
        print(quotes.subject_id)
        println (" " + quotes.object_id.toString)
        
           for (cp <- quotes.same_clusters.filter(p => p!=null)){
               
                val i=cp._2
                val j= cp._1
                if (j>= i- window){
                           println ("cv "+ i.toString + "_" + j.toString)

                 m.addBinaryVar("cv"+ "_" + i.toString() +"_"+ j.toString())
                  vars.add("cv"+ "_" + i.toString() +"_"+ j.toString())
              
                  weights.add (PENALTIES(5).toDouble)  //penalty over mentions
                  var varstmp = new java.util.ArrayList[String]()
                  var weightstmp= new java.util.ArrayList[java.lang.Double]()
                  varstmp.add("v"+ "_" + i.toString() +"_"+ j.toString())
                  varstmp.add("cv"+ "_" + i.toString() +"_"+ j.toString())
                  weightstmp.add(1.0)
                  weightstmp.add(1.0)
                  m.addNormalConstraint(varstmp, weightstmp, "==", 1)
         
              
                }
          
          
          }
          
          
          for (cp <- quotes.different_clusters.filter(p => p!=null)){
                val i=cp._2
                val j= cp._1
                
                if (j>= i- window){
                           println ("cv2 "+ i.toString + "_" + j.toString)

                 m.addBinaryVar("cv2"+ "_" + i.toString() +"_"+ j.toString())
                  vars.add("cv2"+ "_" + i.toString() +"_"+ j.toString())
              
                  weights.add (PENALTIES(5).toDouble)  //penalty over mentions
                  var varstmp = new java.util.ArrayList[String]()
                  var weightstmp= new java.util.ArrayList[java.lang.Double]()
                  varstmp.add("v"+ "_" + i.toString() +"_"+ j.toString())
                  varstmp.add("cv2"+ "_" + i.toString() +"_"+ j.toString())
                  weightstmp.add(1.0)
                  weightstmp.add(-1.0)
                  m.addNormalConstraint(varstmp, weightstmp, "==", 0)
         
              
                }
          
          
          }
      }
    }
    
    
   def computeExactConstraint(score_chart: Array[Array[Float]], txt: Text)  {
    
    // link definite nominal mention to its first occurence.
      for  (i<-1 until score_chart.length) // for mention m_i
        for (j <- i-window  until score_chart(i).length-1)
        {
          
          if (j>=0){
                        if ((txt.predicted_mentions(i).mentionType ==MentionType.NOMINAL) && (txt.predicted_mentions(i).mentionType != MentionType.DEMONSTRATIVE))
                        {
                          
          var x1= txt.predicted_mentions(i).words.toArray.deep.mkString(" ").toLowerCase
           var x2= txt.predicted_mentions(j).words.toArray.deep.mkString(" ").toLowerCase
         
          if (x1 == x2)
              {
               m.addBinaryVar("ex"+ "_" + i.toString() +"_"+ j.toString())
                  vars.add("ex"+ "_" + i.toString() +"_"+ j.toString())
              
                  weights.add (PENALTIES(6).toDouble)  //penalty over mentions
                  var varstmp = new java.util.ArrayList[String]()
                  var weightstmp= new java.util.ArrayList[java.lang.Double]()
                  varstmp.add("v"+ "_" + i.toString() +"_"+ j.toString())
                  varstmp.add("ex"+ "_" + i.toString() +"_"+ j.toString())
                  weightstmp.add(1.0)
                  weightstmp.add(1.0)
                  m.addNormalConstraint(varstmp, weightstmp, "==", 1)
         
              
            }
                        
                        }
              
          }
        }
  }
        
   def sentenceWithoutQuote(txt : Text) = 
     txt.sentences.filter { s => s.words.filter { w => Set("\"", "``", "''" , "'", "`") contains w.form }.length ==0 }
   
   
  // def computeAlignConstraint(txt: Text)
   def computeBackPtsNormal (mId : Int, score_chart : Array[Array[Float]]): Int = score_chart(mId).zipWithIndex.maxBy(_._1)._2
   def computeBackPtsNormalSolution (score_chart : Array[Array[Float]]): Array[Int] = (0 until score_chart.length).toArray.map { mId => computeBackPtsNormal(mId, score_chart) }
  
        
}