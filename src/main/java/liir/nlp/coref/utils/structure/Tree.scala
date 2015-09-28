package liir.nlp.coref.utils.structure
import scala.collection.mutable.ArrayBuffer

case class Tree(var value: String) {

var childs = ArrayBuffer[Tree]()
var parent: Tree = null

def addChildToTheRight (c : Any) {
  c match {
    case s:String => {
      var child = new Tree(s)
      childs :+=child
      child.parent = this
    }
    case tr : Tree => { childs :+= tr
      tr.parent = this }
    
  }
  }

def isLeaf ()  = childs.length == 0

def getAllLeafs() : Array[String] = {
  if (childs.length == 0) return Array[String](value)
    var allchilds= Array[String]()

  for  (c<-childs){
    allchilds = allchilds ++ c.getAllLeafs()
  }
  
  allchilds
}

def getAllLeafNodes() : Array[Tree] = {
  if (childs.length == 0) return Array[Tree](this)
    var allchilds= Array[Tree]()

  for  (c<-childs){
    allchilds = allchilds ++ c.getAllLeafNodes()
  }
  
  allchilds
}

def goUp (levels: Int): Tree = {
  var l = 0
  var rs = this.parent
  while (l<levels){
    rs= rs.parent
    l+=1
  }
  rs
}

def findNode(label: String):Array[Tree] = 
getAllLeafNodes().filter { t => t.value==label }


  
}