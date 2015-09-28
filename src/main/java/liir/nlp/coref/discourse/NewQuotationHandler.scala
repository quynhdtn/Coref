package liir.nlp.coref.discourse
import scala.collection.mutable.ArrayBuffer
import liir.nlp.coref.representation._

import edu.berkeley.nlp.entity.coref.Number
import scala.collection.mutable.Stack

class NewQuotationHandler {
  
    val QUOTATIONS = Set("\"", "``", "''" , "'", "`")
    val OPEN_QUOTATIONS=Set("``",  "`", "\"")
    val CLOSE_QUOTATIONS=Set("''" , "'", "\"")
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
    val  respond_verbs=Set("reply", "answer","respond")
    
    def findQuotation(txt: Text): ArrayBuffer[Quotation]={
            
           txt.sentences.map { s => s.words.map { w => if (w.form == "\"") w.form = w.pos } }
    
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
           var all_quotes=  ArrayBuffer[Quotation]()
           
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
              all_quotes :+= new Quotation(start_pos, end_pos)
              start_pos = null
              end_pos=null
              
              }
            }
   
          }
     
           
     for (q <- all_quotes){
       
       val p = findPredicate(q, txt)
       if (p != null){
         q.predicate = p
         q.predicate_lemma = p._1.sense
         
       }
       
     }
     
     for (q <- all_quotes){
      val sp= findSpeaker(q, txt)
      q.subject_id = sp._1
      q.object_id = sp._2
     }
     
     for (q <- all_quotes){
       val ms = findMentionInQuote(q, txt)
       q.mentions = ms
       
     }
     
     for (q <- all_quotes){
       val os = findObjectInQuote(q, txt)
       q.object_in_quote_id = os
       
     }
     for (q <- all_quotes){
       val os = findWordInQuote(q, txt)
       q.words = os
       
     }
    
     for (i<- all_quotes.length-2 to 1 by -1){
       if (all_quotes(i+1).close_quote._2 >= all_quotes(i).open_quote._2 - 1 )
       {
           if (  all_quotes(i+1).words(all_quotes(i+1).words.length-1).form == "?" && all_quotes(i+1).object_in_quote_id != -1)
             //&&  respond_verbs.filter { x => all_quotes(i).predicate_lemma.startsWith(x)}.size >0 )
           { 
             print ("connect " )
             print (all_quotes(i+1).open_quote)
              print ( "  ")
             print (all_quotes(i+1).subject_id.toString())
                          print ( "  ")
             print ( all_quotes(i+1).object_id.toString())

             print ( " to ")
             println(all_quotes(i).open_quote)
              print ( "  ")
             print (all_quotes(i).subject_id.toString())
                          print ( "  ")
             print ( all_quotes(i).object_id.toString())

             all_quotes(i+1).connectToReply(all_quotes(i))
             
           }
       }
     }
     
     
      for (i<- all_quotes.length-2 to 1 by -1){
       if (all_quotes(i+1).close_quote._2 >= all_quotes(i).open_quote._2 - 1 )
       {
           if (respond_verbs.filter { x => all_quotes(i).predicate_lemma.startsWith(x)}.size >0 )
           { 
             print ("connect " )
             print (all_quotes(i+1).open_quote)
              print ( "  ")
             print (all_quotes(i+1).subject_id.toString())
                          print ( "  ")
             print ( all_quotes(i+1).object_id.toString())

             print ( " to ")
             println(all_quotes(i).open_quote)
              print ( "  ")
             print (all_quotes(i).subject_id.toString())
                          print ( "  ")
             print ( all_quotes(i).object_id.toString())

             all_quotes(i+1).connectToReply(all_quotes(i))
             
           }
       }
     }
      
      for (i<- all_quotes.length-2 to 1 by -1){
       if (all_quotes(i+1).close_quote._2 >= all_quotes(i).open_quote._2 - 1 )
       {
           if (all_quotes(i+1).subject_id != -1 && all_quotes(i).subject_id != -1)
           { 
             print ("connect " )
             print (all_quotes(i+1).open_quote)
              print ( "  ")
             print (all_quotes(i+1).subject_id.toString())
                          print ( "  ")
             print ( all_quotes(i+1).object_id.toString())

             print ( " to ")
             println(all_quotes(i).open_quote)
              print ( "  ")
             print (all_quotes(i).subject_id.toString())
                          print ( "  ")
             print ( all_quotes(i).object_id.toString())

             all_quotes(i+1).connectToReply(all_quotes(i))
             
           }
       }
     }
     
     for (i<- all_quotes.length-2 to 1 by -1){
       if (all_quotes(i+1).close_quote._2 >= all_quotes(i).open_quote._2 - 1 )
       {
         if (all_quotes(i+1).predicate != null && all_quotes(i+1).words(all_quotes(i+1).words.length-1).form != "?" && all_quotes(i+1).object_in_quote_id == -1)
           if (  all_quotes(i).subject_id == -1 &&  all_quotes(i+1).predicate._2 == all_quotes (i+1).close_quote._2 && ( all_quotes(i+1).predicate._1.start_id >= all_quotes (i+1).close_quote._1 ))
             //&&  respond_verbs.filter { x => all_quotes(i).predicate_lemma.startsWith(x)}.size >0 )
           { println("peng")
             all_quotes(i).subject_id = all_quotes(i+1).subject_id}
             
       
       }
     }

     
     for (q <- all_quotes){
       q.exportConstraints(txt)
     }
     all_quotes
     
    }
    
    
    def findPredicate(quote: Quotation , txt: Text): (Predicate[(Int,Int,Int)],Int) = {
      val pred_first_sentence = txt.sentences(quote.open_quote._2).const_predicates.filter(p => communication_verbs.filter{ v => p.sense.startsWith(v)}.size > 0 )

      val pred_end_sentence = txt.sentences(quote.close_quote._2).const_predicates.filter(p => communication_verbs.filter{ v => p.sense.startsWith(v)}.size > 0 )
      
      
        for (pidx <- pred_first_sentence.length-1 to 0 by -1)
        if (pred_first_sentence(pidx).start_id < quote.open_quote._1 &&  pred_first_sentence(pidx).start_id > quote.open_quote._1 - 10)
        
              return (pred_first_sentence(pidx), quote.open_quote._2)
          
        
       for (pidx <- 0 until pred_end_sentence.length)
       if (pred_end_sentence(pidx).start_id > quote.close_quote._1 && pred_end_sentence(pidx).start_id < quote.close_quote._1 + 10)
             return (pred_end_sentence(pidx), quote.close_quote._2)
      
        
       return null
         
    }
    
    
    def findSpeaker(quote: Quotation,  txt: Text) : (Int,Int) = {
       var char_subject = -1
       var char_object = -1
      if (quote.predicate != null){
       val sen = txt.sentences(quote.predicate._2)
       val pred = quote.predicate._1
      
      
       val subjects = sen.getMentions.filter { m => sen.getSemanticLabel(quote.predicate._1, m.startIdx, m.endIdx) == "A0" }
       val objects = sen.getMentions.filter { m => sen.getSemanticLabel(quote.predicate._1, m.startIdx, m.endIdx) == "A2" }
   
       if (subjects.length==1){
            if (char_subject == -1)
            char_subject=  subjects(0).mentIdx
          }
          
       if (objects.length==1){
            char_object = objects(0).mentIdx
          }
       
       
       if (char_object == -1)
       {
         
         val   myobjects = sen.getMentions.filter { m => 
            if (pred.start_id < sen.words.length){
               if (m.startIdx == pred.end_id +1 &&    sen.words(pred.end_id).pos == "TO")
                 true
               else
                 false
            }
            else
              false
          
          }
         
          if (myobjects.length >=1)
              char_object = myobjects(myobjects.length-1).mentIdx
       }
       
      }
       if (char_subject == -1)
       {
         
         if (quote.close_quote._1 < txt.sentences(quote.close_quote._2).words.length-2)
         if (communication_verbs contains txt.sentences(quote.close_quote._2).words(quote.close_quote._1+1).lemma){
           val sbs = txt.predicted_mentions.filter { m => m.sentIdx == quote.close_quote._2 && m.startIdx == quote.close_quote._1 +2 }
            if (sbs.length >=1)
              {
                char_subject = sbs(sbs.length-1).mentIdx
                quote.predicate_lemma = txt.sentences(quote.close_quote._2).words(quote.close_quote._1+1).lemma
              }
          
         }
       }
      
       if (char_subject != -1)
         if (char_subject == char_object)
           char_object = -1
     (char_subject, char_object)
      
      
    }
  
    
    
    def findMentionInQuote(quote:Quotation, txt: Text): Array[Int]={
      var mentions = Array[Int]()
      
      
      if (quote.open_quote._2 == quote.close_quote._2){
      val m1 = txt.sentences(quote.open_quote._2).getMentions.filter { m => m.startIdx  > quote.open_quote._1 && m.endIdx < quote.close_quote._1 }    
      
      for (m<-m1)  
        mentions :+= m.mentIdx
      
      
      }
      
      else{
       val m1 =   txt.sentences(quote.open_quote._2).getMentions.filter { m => m.startIdx  > quote.open_quote._1}
       for (m<-m1)  
          mentions :+= m.mentIdx
          
       val m2 =   txt.sentences(quote.close_quote._2).getMentions.filter { m => m.endIdx  < quote.close_quote._1}
       for (m<-m2)  
          mentions :+= m.mentIdx
          
       for (j<-quote.open_quote._2+1 until quote.close_quote._2 )
         {val m3 =   txt.sentences(j).getMentions
         for (m<-m3)  
          mentions :+= m.mentIdx
         
         }
      }

      mentions
    }
    
    
    def findObjectInQuote(quote:Quotation, txt: Text):Int={
      val words_in_quote = txt.sentences(quote.open_quote._2).words.filter { w => txt.sentences(quote.open_quote._2).words.indexOf(w) > quote.open_quote._1  } ++ txt.sentences.filter{ s => txt.sentences.indexOf(s) > quote.open_quote._2 &&  txt.sentences.indexOf(s) < quote.close_quote._2  }.flatMap { s => s.words } ++ txt.sentences(quote.close_quote._2).words.filter { w => txt.sentences(quote.close_quote._2).words.indexOf(w) < quote.close_quote._1  } 
      
      if (words_in_quote.length>=2){
      
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
      -1
    
    }
    
    def findWordInQuote(quote:Quotation, txt: Text):ArrayBuffer[Word]={
       txt.sentences(quote.open_quote._2).words.filter { w => txt.sentences(quote.open_quote._2).words.indexOf(w) > quote.open_quote._1  } ++ txt.sentences.filter{ s => txt.sentences.indexOf(s) > quote.open_quote._2 &&  txt.sentences.indexOf(s) < quote.close_quote._2  }.flatMap { s => s.words } ++ txt.sentences(quote.close_quote._2).words.filter { w => txt.sentences(quote.close_quote._2).words.indexOf(w) < quote.close_quote._1  } 
      
      
    
    }
    
}