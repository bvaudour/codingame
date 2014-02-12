package resistance

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.collection.mutable.{HashMap => Map, HashSet => Set, MutableList => List, Stack}

object Solution {

  val encodage = {
    val out = new Map[Char, String]
    out.put('A', ".-")
    out.put('B', "-...")
    out.put('C', "-.-.")
    out.put('D', "-..")
    out.put('E', ".")
    out.put('F', "..-.")
    out.put('G', "--.")
    out.put('H', "....")
    out.put('I', "..")
    out.put('J', ".---")
    out.put('K', "-.-")
    out.put('L', ".-..")
    out.put('M', "--")
    out.put('N', "-.")
    out.put('O', "---")
    out.put('P', ".--.")
    out.put('Q', "--.-")
    out.put('R', ".-.")
    out.put('S', "...")
    out.put('T', "-")
    out.put('U', "..-")
    out.put('V', "...-")
    out.put('W', ".--")
    out.put('X', "-..-")
    out.put('Y', "-.--")
    out.put('Z', "--..")
    out
  }
  val dic = new Set[String]

  def main(args: Array[String]) {
    // Lecture des données
    val (seq, n) = (readLine, readInt)
    for (i <- 0 until n) dic += encode(readLine)

    // Calcul du nombre de possibilités
    println(compute(seq))
  }

  def encode(s: String): String = {
    val b = new StringBuilder
    for (c <- s) b ++= encodage(c)
    b.toString
  }

  def compute(seq: String): Long = {
    val s = seq.size
    val res = new Array[Long](s + 1)
    res(0) = 1
    val maxlength = dic.map(_.size).max
    for (i <- 1 to s) {
      var nb: Long = 0
      val start = max(0, i - maxlength)
      for (j <- start until i) {
        if (dic.contains(seq.substring(j, i))) nb += res(j)
      }
      res(i) = nb
    }
    return res(s)
  }
}
