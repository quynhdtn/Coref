package liir.nlp.coref.utils
import java.io._
import scala.io.Source._

import java.util.zip._
import java.io.File
import java.io.PrintWriter
class FileUtilScala {

  private val buf = new Array[Byte](1024)
  
  def compress(path: String) {
    val src = new File(path)
    val dst = new File(path ++ ".gz")
 
    try {
      val in  = new BufferedInputStream(new FileInputStream(src))
      try {
        val out = new GZIPOutputStream(new FileOutputStream(dst))
          try {
            var n = in.read(buf)
            while (n >= 0) {
              out.write(buf, 0, n)
              n = in.read(buf)
              }
            }
          finally {
            out.flush
            out.close()
          }
          } 
      catch {
            case _:FileNotFoundException =>
              System.err.printf("Permission Denied: %s", path ++ ".gz")
            case _:SecurityException =>
              System.err.printf("Permission Denied: %s", path ++ ".gz")
      }
    } 
    catch {
      case _: FileNotFoundException =>
        System.err.printf("File Not Found: %s", path)
      case _: SecurityException =>
        System.err.printf("Permission Denied: %s", path)
    }
  }
  def decompress(inf: String, outf: String) {
    val src = new File(inf)
    val dst = new File(outf)
 
    try {
      val in  = new GZIPInputStream(new FileInputStream(src))
      try {
  val out = new BufferedOutputStream(new FileOutputStream(dst))
  try {
    var n = in.read(buf)
    while (n >= 0) {
      out.write(buf, 0, n)
      n = in.read(buf)
    }
  }
  finally {
    out.flush
    out.close
  }
      } catch {
  case _:FileNotFoundException =>
    System.err.printf("Permission Denied: %s", inf)
  case _:SecurityException =>
    System.err.printf("Permission Denied: %s", inf)
      }
    } catch {
      case _: FileNotFoundException =>
  System.err.printf("File Not Found: %s", inf)
      case _: SecurityException =>
  System.err.printf("Permission Denied: %s", inf)
    }
  }
  
   def deleteFile(filename: String) = { new File(filename).delete() }
   
   def combineTwoFileWithColumns ( f1: String, f2: String, out: String){
     
     
        val lines1 = fromFile(f1).getLines.toList 
        val lines2 = fromFile(f2).getLines().toList
        
        val lines1f = lines1.filter { l => l.trim != "" }
        val lines2f = lines2.filter { l => l.trim != "" }
 //       require (lines1.length == lines2.length)
        
        var writer = new PrintWriter(new File(out))
        
        var id =0
    for (i <- 0 until lines1.length)
    {
      var line1 = lines1(i).trim
   //   var line2= lines2(i).trim
      if (line1!=""){
      if (line1.startsWith("#")) 
        writer.write(line1+"\n")
        else{
      
      var tmps1 = line1.split ("\\s+")
      var tmps2= lines2f(id).split("\\s+")
      if (tmps1.length >0){
      for (j<- 0 until tmps1.length-1)
        writer.write(tmps1(j) + "\t")
      //  writer.write(tmps2(2) + "\t")
      //  for (j<- 3 until tmps1.length-1)
     //   writer.write(tmps1(j) + "\t")
      writer.write(tmps2(tmps2.length-1) + "\n")}
      else
        writer.write("\n")
        }
      id+=1
      }
      else{
        writer.write("\n")
      }
    }    
      
     writer.close()
   }
  
}