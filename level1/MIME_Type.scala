package mime

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.collection.mutable.HashMap

object Solution {
   def main(args: Array[String]) {
     // Lecture nombre d'éléments
     val (n, q) = (readInt, readInt)
     // Lecture associations
     val mime = new HashMap[String, String]()
     for (i <- 0 until n) {
       val s = readLine().split(" ")
       mime(s(0).toLowerCase) = s(1)
     }
     // Lecture extensions fichiers + impression
     for (i <- 0 until q) {
       var s = readLine
       val idx = s.lastIndexOf('.')
       s = if (idx < 0 || idx == s.size - 1) "" else s.substring(idx + 1).toLowerCase
       mime.get(s) match {
         case Some(m) => println(m)
         case None    => println("UNKNOWN")
       }
     }
   }
 }
