package liir.nlp.coref.discourse
import liir.nlp.coref.representation._
import edu.berkeley.nlp.entity.coref.Number
import scala.collection.mutable.Stack
class QuotationHandler {
  
    val QUOTATIONS = Set("\"", "``", "''" , "'", "`")
    val OPEN_QUOTATIONS=Set("``",  "`")
    val CLOSE_QUOTATIONS=Set("''" , "'")
    val  communication_verbs=Set("alert", "assure", "brief", "educate", "encourage", "inform", "notify", "admonish", "advise", "caution", "counsel", "instruct", "warn", "argue", "chat", "chatter", "chitchat", "confer", "converse", "debate", "gab", "gossip", "jest", "joke", "palaver", "rap", "schmooze", "yak", "bellyache", "bitch", "boast", "brag", "caterwaul", "complain", "crab", "gripe", "grouch", "grouse", "grumble", "kvetch", "moan", "object", "whine", "confess", "admit", "acknowledge", "fess_up", "proclaim", "reveal", "ask", "enquire", "inquire", "consult", "pry", "broadcast", "cable", "e-mail", "fax", "modem", "netmail", "phone", "radio", "relay", "satellite", "semaphore", 
    "sign", "signal", "telecast", "telegraph", "telephone", "telex", "wire", "wireless", "cross-examine", "question", "interview", "interrogate", "lecture", "moralize", "preach", 
    "rant", "remark", "speak", "talk", "testify", "theorize", "write", "pontificate", "elaborate", "comment", "dwell",
    "babble", "bark", "bawl", "bellow", "bleat", "blubber", "boom", "bray", "burble", "bluster", "cackle", "call", 
    "carol", "chant", "chatter", "chirp", "chortle", "chuckle", "cluck", "coo", "croak", "croon", "crow", "cry", 
    "drawl", "drone", "gabble", "gasp", "gibber", "groan", "growl", "grumble", "grunt", "hiss", "holler", "hoot",
    "howl", "jabber", "keen", "lilt", "lisp", "mewl", "moan", "mumble", "murmur", "mutter", "nasal", "natter", "pant",
    "prattle", "purr", "quaver", "rage", "rant", "rasp", "roar", "rumble", "scream", "screech", "shout", "shriek",
    
    "sibilate", "simper", "sigh", "sing", "smatter", "smile", "snap", "snarl", "snivel", "snuffle", "splutter",
    "squall", "squawk", "squeak", "squeal", "stammer", "stemmer", "stutter", "thunder", "tisk", "trill", "trumpet",
    "twang", "twitter", "vociferate", "wail", "warble", "wheeze", "whimper", "whine", "whisper", "whistle", "witter",
    "whoop", "yammer", "yap", "yell", "yelp", "yodel", "blare", "gurgle", "hum", "overstate", "overdraw",
    "hyperbolize", "magnify", "amplify", "overemphasize", "overrate", "overstress", "tout", "glorify", "assure",
    "guarantee", "promise", "ascertain", "add", "allege", "blabber", "disclose", "divulge", "exclaim", "insinuate", 
    "insist", "intimate", "leak", "reply", "respond", "retort", "promulgate", "utter", "venture", "vocalize", "voice",
    "volunteer", "write", "advise", "announce", "articulate", "blab", "blurt", "claim", "confess", "confide", "convey",
    "declare", "interject", "interpose", "mention", "observe", "proclaim", "propose", "recount", "reiterate", "relate",
    "reveal", "say", "state", "volunteer", "repeat", "remark", "note", "report", "purpose", "suggest", "recommend", 
    "hint", "speak", "talk", "acquaint", "inform", "apprise", "notify", "advise", "update", "remind", "tell",
    "demonstrate", "elucidate", "explain", "explicate", "expound", "justify", "narrate", "pose", "preach",
    "recap", "recite", "relay", "sniff", "illustrate", "verify", "corroborate", "communicate", "outline",
    "summarize", "sum_up", "boil_down", "snitch", "dictate", "quote", "read", "show", "teach", "tell", "write",
    "command", "ask", "order", "require", "demand", "allow", "beseech", "call", "hail", "invite", "okay", "permit",
    "summon", "add", "think","enquire")
    
    
    val  thinking_verbs=Set("think", "believe","decide", "remember", "imagine")
        val  respond_verbs=Set("reply", "answer","respond")

    def use_open_quotation(txt: Text):Boolean = txt.sentences.foldLeft(0)((c,s)=>c+s.words.count { w => OPEN_QUOTATIONS contains w.form }) >0
    
    def handler(txt: Text){
     println(txt.id)
     val num_quote =  txt.sentences.foldLeft(0)((c,s)=>c+s.words.count { w => QUOTATIONS contains w.form })
     if (num_quote % 2 != 0)
     {
       println (num_quote)
       println ("error in quoting!!!")
     }
      
     var quote_position = Stack[Tuple3[Int,Int, String]]()
     
     txt.sentences.map { s => s.words.map { w =>  
       if ( QUOTATIONS contains w.form)  
         quote_position.push((s.words.indexOf(w), txt.sentences.indexOf(s), w.form))
         }
     }
     
     
     var has_quote = false
    
     var end_pos:Tuple2[Int,Int]= null
     var start_pos:Tuple2[Int,Int] =null
     var all_quotes= Array[Tuple2[Tuple2[Int,Int], Tuple2[Int,Int]]]()
     
    while (! quote_position.isEmpty){
      val t = quote_position.pop()
      if (CLOSE_QUOTATIONS contains t._3){
        if (!has_quote){
        has_quote=true
        end_pos = (t._1, t._2)
        }
      }
      
       if (OPEN_QUOTATIONS contains t._3){
        if (has_quote){
        has_quote=false
        start_pos = (t._1, t._2)
        all_quotes :+=(start_pos, end_pos)
        start_pos = null
        end_pos=null
        
        }
      }
      
      
      
    }
     
     
     
     println (all_quotes.deep.mkString("\t"))
     
     for (quote<- all_quotes){
       val s = findSpeaker(quote, txt)
       if (s != -1)
       {
         
         print("Found speaker! ")
         print(quote)
         print(": ")
         println(s)
       }
       
     }
     
    }

    
    
     def reference(txt: Text): (Array[(Int,Int)],Array[(Int,Int)])={
     val num_quote =  txt.sentences.foldLeft(0)((c,s)=>c+s.words.count { w => QUOTATIONS contains w.form })
    
      
     var quote_position = Stack[Tuple3[Int,Int, String]]()
     
     txt.sentences.map { s => s.words.map { w =>  
       if ( QUOTATIONS contains w.form)  
         quote_position.push((s.words.indexOf(w), txt.sentences.indexOf(s), w.form))
         }
     }
     
     
     var has_quote = false
    
     var end_pos:Tuple2[Int,Int]= null
     var start_pos:Tuple2[Int,Int] =null
     var all_quotes= Array[Tuple2[Tuple2[Int,Int], Tuple2[Int,Int]]]()
     
    while (! quote_position.isEmpty){
      val t = quote_position.pop()
      if (CLOSE_QUOTATIONS contains t._3){
        if (!has_quote){
        has_quote=true
        end_pos = (t._1, t._2)
        }
      }
      
       if (OPEN_QUOTATIONS contains t._3){
        if (has_quote){
        has_quote=false
        start_pos = (t._1, t._2)
        all_quotes :+=(start_pos, end_pos)
        start_pos = null
        end_pos=null
        
        }
      }
      
      
      
    }
     
     var same_clusters = Array[(Int,Int)]()
     var different_clusters = Array[(Int,Int)]()
/*
     for (quote<- all_quotes){
       val p = all_quotes.indexOf(quote)
       if (p<all_quotes.length-1)
       {
         if (all_quotes(p+1)._2._2 >= quote._1._2 -1){
           val current_speaker = findSpeakerSpecial(quote,txt, respond_verbs)
           if (current_speaker._1!= -1){
             val prev_mentions = findMentionInQuote(all_quotes(p+1), txt)
               
               
             for (m <- prev_mentions){
               if (Set("you", "your", "yours" ) contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase ){
                 println( "pron " + m.toString() + "_" + current_speaker._1.toString())
                 if (current_speaker._1 < m)
                 same_clusters :+=(current_speaker._1, m)
               else   if (m < current_speaker._1)
                                same_clusters :+=(m,current_speaker._1)
    
               }
            
               
             }
           }
         }
       }
       
     }*/
     for (quote<- all_quotes){
       var s = findSpeaker(quote, txt)
       val mentions = findMentionInQuote(quote, txt)
       
       // find related speaker
          /*
                if (s._1 == -1){
                  val p = all_quotes.indexOf(quote)
                  if (p<all_quotes.length-1){
                    if (all_quotes(p+1)._2._2 >= quote._1._2-1){
                    val prev_speaker = findSpeakerSpecial(all_quotes(p+1),txt, thinking_verbs)
                    println ("special")
                    if (prev_speaker._1 != -1)
                      s = (prev_speaker._1, s._2)
                    
                    
                    }
                    
                    
                    
                  }
                }
                * 
                */
       if (s._1!= -1){
         
         for (m <- mentions){
           if (Set("i", "me", "my", "mine" ) contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase ){
             if (s._1 < m)
             same_clusters :+=(s._1, m)
           else  if (m < s._1)
                            same_clusters :+=(m,s._1)

           }
           
           else{
             
             if (Set("we", "our", "ours") contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase ){
             
               if (txt.predicted_mentions(s._1).number == Number.PLURAL){
               
                 if (s._1 < m)
               same_clusters :+=(s._1, m)
             else  if (m < s._1)
                              same_clusters :+=(m,s._1)

           }
             }
               else{
             
             if (s._1 < m)
             different_clusters :+=(s._1, m)
           else   if (m < s._1)
             different_clusters :+=(m,s._1)
             
           }
               
             
             
           }
           
           
         }
         
         
       }
          if (s._2!= -1){
         
             for (m <- mentions){
               if (Set("you", "your", "yours" ) contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase ){
                 if (s._2 < m)
                 same_clusters :+=(s._2, m)
               else   if (m < s._2)
                                same_clusters :+=(m,s._2)
    
               }
               
               else{
                 
                 if (s._2 < m)
                 different_clusters :+=(s._2, m)
             else    if (m < s._2)
                 different_clusters :+=(m,s._2)
                 
               }
               
           
           
             }
         
         
             }
          
          val obj_in_quote = findObjectInQuote(quote, txt)
       
          if (obj_in_quote != -1){
            
            println ("find obj in quote")
            println (obj_in_quote)
            
             for (m <- mentions){
               println(txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase)
               if (Set("you", "your", "yours" ) contains  txt.predicted_mentions(m).words.toArray.deep.mkString(" ").toLowerCase ){
                 {  
                   print ("added!")
                   print (m)
                   println ("_ " + obj_in_quote.toString())
                   
                   if (obj_in_quote < m)
                 same_clusters :+=(obj_in_quote, m)
               else  if (m < obj_in_quote)
                                same_clusters :+=(m,obj_in_quote)
                 }
               }
               
               else{
                 
                 if (obj_in_quote < m)
                 different_clusters :+=(obj_in_quote, m)
               else  if (m < obj_in_quote)
                 different_clusters :+=(m,obj_in_quote)
                 
               }
               
           
           
             }
             
             if (s._2 != -1)
             { if (s._2 < obj_in_quote)
                 same_clusters :+=(s._2, obj_in_quote)
               else  if (obj_in_quote < s._2)
                                same_clusters :+=(obj_in_quote ,s._2) }
               
          }
          
          
          
                
           
         }
     
    
     (same_clusters, different_clusters)
     
    }
    
     
     
    def findSpeakerSpecial(quote: Tuple2[Tuple2[Int,Int], Tuple2[Int,Int]], txt: Text, verbSet:Set[String]): (Int, Int)={
    
      val pred_first_sentence = txt.sentences(quote._1._2).const_predicates.filter(p => verbSet.filter{ v => p.sense.startsWith(v)}.size > 0 )

      val pred_end_sentence = txt.sentences(quote._2._2).const_predicates.filter(p => verbSet.filter{ v => p.sense.startsWith(v)}.size > 0 )
      
      
      var char_subject = -1
      var char_object = -1
      
      for (pidx <- pred_first_sentence.length-1 to 0 by -1){
        if (pred_first_sentence(pidx).end_id < quote._1._1)
        {
          val subjects = txt.sentences(quote._1._2).getMentions.filter { m => txt.sentences(quote._1._2).getSemanticLabel(pred_first_sentence(pidx), m.startIdx, m.endIdx) == "A0" }

          val objects = txt.sentences(quote._1._2).getMentions.filter { m => 
            if (pred_first_sentence(pidx).end_id < txt.sentences(quote._1._2).words.length){
               if (m.startIdx == pred_first_sentence(pidx).end_id +1 &&    txt.sentences(quote._1._2).words(pred_first_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }

          if (subjects.length==1){
            char_subject = subjects(0).mentIdx
          }
          
          if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
      
      
      for (pidx <- 0 until pred_end_sentence.length){
        if (pred_end_sentence(pidx).end_id > quote._2._1)
        {
          val subjects = txt.sentences(quote._2._2).getMentions.filter { m => txt.sentences(quote._2._2).getSemanticLabel(pred_end_sentence(pidx), m.startIdx, m.endIdx) == "A0" }
          val objects = txt.sentences(quote._2._2).getMentions.filter { m => 
            if (pred_end_sentence(pidx).end_id < txt.sentences(quote._2._2).words.length){
               if (m.startIdx == pred_end_sentence(pidx).end_id +1 &&    txt.sentences(quote._2._2).words(pred_end_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }
          
          if (subjects.length==1){
            if (char_subject == -1)
            char_subject=  subjects(0).mentIdx
          }
          
           if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
      
      
      
      for (pidx <- pred_first_sentence.length-1 to 0 by -1){
        if (pred_first_sentence(pidx).end_id < quote._1._1)
        {
          
          
          
          val subjects = txt.sentences(quote._1._2).getMentions.filter { m => m.endIdx == pred_first_sentence(pidx).start_id }      
          
          val objects = txt.sentences(quote._1._2).getMentions.filter { m => 
            if (pred_first_sentence(pidx).end_id < txt.sentences(quote._1._2).words.length){
               if (m.startIdx == pred_first_sentence(pidx).end_id +1 &&    txt.sentences(quote._1._2).words(pred_first_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }
          if (subjects.length>=1){
            if (char_subject == -1)
            char_subject=  subjects(subjects.length-1).mentIdx
          }
          
           if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
      
      
       for (pidx <- 0 until pred_end_sentence.length){
        if (pred_end_sentence(pidx).end_id > quote._2._1)
        {
          val subjects = txt.sentences(quote._2._2).getMentions.filter { m => m.endIdx == pred_end_sentence(pidx).start_id || m.startIdx == pred_end_sentence(pidx).end_id }
           val objects = txt.sentences(quote._2._2).getMentions.filter { m => 
            if (pred_end_sentence(pidx).end_id < txt.sentences(quote._2._2).words.length){
               if (m.startIdx == pred_end_sentence(pidx).end_id +1 &&    txt.sentences(quote._2._2).words(pred_end_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }
          if (subjects.length>=1){
            if (char_subject == -1)
            char_subject=  subjects(subjects.length-1).mentIdx
         }
          
          if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
       
      
      (char_subject, char_object)
      
      
    }

    
    def findSpeaker(quote: Tuple2[Tuple2[Int,Int], Tuple2[Int,Int]], txt: Text): (Int, Int)={
    
      val pred_first_sentence = txt.sentences(quote._1._2).const_predicates.filter(p => communication_verbs.filter{ v => p.sense.startsWith(v)}.size > 0 )

      val pred_end_sentence = txt.sentences(quote._2._2).const_predicates.filter(p => communication_verbs.filter{ v => p.sense.startsWith(v)}.size > 0 )
      
      
      var char_subject = -1
      var char_object = -1
      
      for (pidx <- pred_first_sentence.length-1 to 0 by -1){
        if (pred_first_sentence(pidx).end_id < quote._1._1)
        {
          val subjects = txt.sentences(quote._1._2).getMentions.filter { m => txt.sentences(quote._1._2).getSemanticLabel(pred_first_sentence(pidx), m.startIdx, m.endIdx) == "A0" }

          val objects = txt.sentences(quote._1._2).getMentions.filter { m => 
            if (pred_first_sentence(pidx).end_id < txt.sentences(quote._1._2).words.length){
               if (m.startIdx == pred_first_sentence(pidx).end_id +1 &&    txt.sentences(quote._1._2).words(pred_first_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }

          if (subjects.length==1){
            char_subject = subjects(0).mentIdx
          }
          
          if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
      
      
      for (pidx <- 0 until pred_end_sentence.length){
        if (pred_end_sentence(pidx).end_id > quote._2._1)
        {
          val subjects = txt.sentences(quote._2._2).getMentions.filter { m => txt.sentences(quote._2._2).getSemanticLabel(pred_end_sentence(pidx), m.startIdx, m.endIdx) == "A0" }
          val objects = txt.sentences(quote._2._2).getMentions.filter { m => 
            if (pred_end_sentence(pidx).end_id < txt.sentences(quote._2._2).words.length){
               if (m.startIdx == pred_end_sentence(pidx).end_id +1 &&    txt.sentences(quote._2._2).words(pred_end_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }
          
          if (subjects.length==1){
            if (char_subject == -1)
            char_subject=  subjects(0).mentIdx
          }
          
           if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
      
      
      
      for (pidx <- pred_first_sentence.length-1 to 0 by -1){
        if (pred_first_sentence(pidx).end_id < quote._1._1)
        {
          
          
          
          val subjects = txt.sentences(quote._1._2).getMentions.filter { m => m.endIdx == pred_first_sentence(pidx).start_id }      
          
          val objects = txt.sentences(quote._1._2).getMentions.filter { m => 
            if (pred_first_sentence(pidx).end_id < txt.sentences(quote._1._2).words.length){
               if (m.startIdx == pred_first_sentence(pidx).end_id +1 &&    txt.sentences(quote._1._2).words(pred_first_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }
          if (subjects.length>=1){
            if (char_subject == -1)
            char_subject=  subjects(subjects.length-1).mentIdx
          }
          
           if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
      
      
       for (pidx <- 0 until pred_end_sentence.length){
        if (pred_end_sentence(pidx).end_id > quote._2._1)
        {
          val subjects = txt.sentences(quote._2._2).getMentions.filter { m => m.endIdx == pred_end_sentence(pidx).start_id || m.startIdx == pred_end_sentence(pidx).end_id }
           val objects = txt.sentences(quote._2._2).getMentions.filter { m => 
            if (pred_end_sentence(pidx).end_id < txt.sentences(quote._2._2).words.length){
               if (m.startIdx == pred_end_sentence(pidx).end_id +1 &&    txt.sentences(quote._2._2).words(pred_end_sentence(pidx).end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }
          if (subjects.length>=1){
            if (char_subject == -1)
            char_subject=  subjects(subjects.length-1).mentIdx
         }
          
          if (objects.length==1){
            char_object = objects(0).mentIdx
          }
        }
      }
       
       if (char_subject == -1)
         char_subject = findSubjectByTemplate(quote, txt)
      
      (char_subject, char_object)
      
      
    }
    
    
     def findSubjectByTemplate(quote:((Int,Int),(Int,Int)), txt: Text):Int={
       if (quote._2._1 < txt.sentences(quote._2._2).words.length-2)
       if (communication_verbs contains txt.sentences(quote._2._2).words(quote._2._1+1).lemma)
       {
        val sbs = txt.predicted_mentions.filter { m => m.sentIdx == quote._2._2 && m.startIdx == quote._2._1 +2 }
        if (sbs.length ==1)
          return sbs(0).mentIdx
       }
        
      -1
      
    
    }
    
    def findObjectInQuote(quote:((Int,Int),(Int,Int)), txt: Text):Int={
      val words_in_quote = txt.sentences(quote._1._2).words.filter { w => txt.sentences(quote._1._2).words.indexOf(w) > quote._1._1  } ++ txt.sentences.filter{ s => txt.sentences.indexOf(s) > quote._1._2 &&  txt.sentences.indexOf(s) < quote._2._2  }.flatMap { s => s.words } ++ txt.sentences(quote._2._2).words.filter { w => txt.sentences(quote._2._2).words.indexOf(w) < quote._2._1  } 
      
      
      val objs = txt.predicted_mentions.filter { m => if (m.startIdx >=1){
        if (
        txt.sentences(m.sentIdx).words(m.endIdx-1) == words_in_quote (words_in_quote.length-2) &&
       txt.sentences(m.sentIdx).words(m.startIdx-1).form == "," && words_in_quote (words_in_quote.length-1).pos == "." )
          true
        else
          false}
      else
        false
      }
      
      if (objs.length==1)
        return objs(0).mentIdx
      else
        -1
    
    
    
    }
    
    def findMentionInQuote(quote:((Int,Int),(Int,Int)), txt: Text): Array[Int]={
      var mentions = Array[Int]()
      
      
      if (quote._1._2 == quote._2._2){
      val m1 = txt.sentences(quote._1._2).getMentions.filter { m => m.startIdx  > quote._1._1 && m.endIdx < quote._2._1 }    
      
      for (m<-m1)  
        mentions :+= m.mentIdx
      
      
      }
      
      else{
       val m1 =   txt.sentences(quote._1._2).getMentions.filter { m => m.startIdx  > quote._1._1}
       for (m<-m1)  
          mentions :+= m.mentIdx
          
       val m2 =   txt.sentences(quote._2._2).getMentions.filter { m => m.endIdx  < quote._2._1}
       for (m<-m2)  
          mentions :+= m.mentIdx
          
       for (j<-quote._1._2+1 until quote._2._2 )
         {val m3 =   txt.sentences(j).getMentions
         for (m<-m3)  
          mentions :+= m.mentIdx
         
         }
      }

      mentions
    }

}