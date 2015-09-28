package liir.nlp.coref.discourse
import liir.nlp.coref.representation._

object Conversation {
  
  
  val CONVERSATION_WORDS_STRONG= Set("Did", "Is", "Please",
       "Oh","Hmm",  "Sorry", "Well", "OK", "Ok", "Yeah","Right", "Do",
      "Let", "Anyway", "Are", "Uh", "Mmm", "Sure", "Really", "Uh-huh", "Hmmm", "Hey", "Yes" )
      
  val CONVERSATION_WORDS= Set("oh", "no", "yes", ":", "\"", "``", "!", "?", "What", "''", "I", "you", "Did", "we", "us", "Is", "Please",
      "You", "i", "We", "Which", "When", "Where", "Why", "Oh","Hmm", "How", "Did", "Sorry", "Well", "OK", "Ok", "Yeah","Right",
      "Let", "Anyway", "Are", "Uh", "Mmm", "Sure", "Really", "Uh-huh", "Hmmm", "Hey", "Yes", "Thank", "thank", "me", "my", "your", "our",
      "May", "Might", "Could","Can", "Maybe", "Do")   
      
  val QUOTATIONS = Set("\"", "``", "''" , "'", "`")
  
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
    "summon", "add")

  
  def detectSpeech (s: Sentence): (Array[Tuple2[Int,Int]], Array[Tuple2[Int,Int]]) = {
    
    val preds = s.const_predicates.filter(p => communication_verbs.filter{ v => p.sense.startsWith(v)}.size > 0 )
    val quotation_positions = s.words.filter { w => QUOTATIONS contains w.form }.map { w =>s.words.indexOf(w) }
    val mentions= s.getMentions
    val prons = s.getPronMentions
    var rs = Array[Tuple2[Int,Int]]()
        var rs2 = Array[Tuple2[Int,Int]]()

    if (quotation_positions.length != 0){
      
      for (p <- preds.length-1 to 0 by -1){
      
        if (preds(p).start_id < quotation_positions(0))
        {
          val subjects = mentions.filter { m => s.getSemanticLabel(preds(p), m.startIdx, m.endIdx) == "A0" }
          if (subjects.length==1){
          var end_quote= s.words.length
          if (quotation_positions.length >1){
            end_quote= quotation_positions(1)
          }
     
          for (pp <- prons){
            if (Set("i", "my", "mine") contains pp.words.toArray.deep.mkString(" ").toLowerCase){
            if (pp.startIdx > quotation_positions(0) && (pp.endIdx < end_quote)){
              rs :+= (pp.mentIdx, subjects(0).mentIdx)
            }}
            
             if (Set("you", "your", "yours", "he", "his", "him", "she", "her", "their", "them", "they", "it", "its", "theirs") contains pp.words.toArray.deep.mkString(" ").toLowerCase){
            if (pp.startIdx > quotation_positions(0) && (pp.endIdx < end_quote)){
              rs2 :+= (pp.mentIdx, subjects(0).mentIdx)
            }
             }
          }
          
        }
        }
      }
        if  (quotation_positions.length >= 3){
          
          
           for (p <- preds.length-1 to 0 by -1){
      
        if (preds(p).start_id < quotation_positions(2))
        {
          val subjects = mentions.filter { m => s.getSemanticLabel(preds(p), m.startIdx, m.endIdx) == "A0" }
          if (subjects.length==1){
          var end_quote= s.words.length
          if (quotation_positions.length >3){
            end_quote= quotation_positions(3)
          }
     
          for (pp <- prons){
            if (Set("i", "my", "mine") contains pp.words.toArray.deep.mkString(" ").toLowerCase){
            if (pp.startIdx > quotation_positions(2) && (pp.endIdx < end_quote)){
              rs :+= (pp.mentIdx, subjects(0).mentIdx)
            }}
            
             if (Set("you", "your", "yours", "he", "his", "him", "she", "her", "their", "them", "they", "it", "its", "theirs") contains pp.words.toArray.deep.mkString(" ").toLowerCase){
            if (pp.startIdx > quotation_positions(2) && (pp.endIdx < end_quote)){
              rs2 :+= (pp.mentIdx, subjects(0).mentIdx)
            }
             }
          }
          
        }
        }
      }
           
        }
      
    
    }
    (rs,rs2)
  }
  
  def isSpeech (s: Sentence): Boolean = {
    
    if ((QUOTATIONS contains  s.words(0).form ) || (QUOTATIONS contains  s.words(s.words.length-1).form ))
    return true
    
    if (s.words.map { w => w.form } contains  ":")
      return true
    
    if (countConversationWords(s, CONVERSATION_WORDS_STRONG) >=1)
      return true
    
    if (countConversationWords(s, CONVERSATION_WORDS) >=2)
      return true
    false
    
    
  }

  def countConversationWords (s: Sentence, myset:Set[String]) = s.words.map { w => w.form }.count { wf => myset contains wf }
  def findConversation(t: Text):Array[Int]={   
    var rs = Array[Int]()
    var in_conversation = false
    
    for (i<- 0 until t.sentences.length-1){
      
    
        if (isSpeech (t.sentences(i)))
        {
          rs:+=i
          in_conversation = true
        }
   
        else
        {
          if (in_conversation){
           if (isSpeech (t.sentences(i+1)))
            {
            rs:+=i
            in_conversation = true
            }
           else
             in_conversation = false  
          }
          else
            in_conversation = false
           
        }
  
    }
    
    rs
    
  }
  
  
}