package telephone_numbers

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val n = readInt
    val tab = new Array[String](n)
    for (i <- 0 until n)
      tab(i) = readLine
    // Calcul du nombre d'emplacements mémoires
    val tS = tab.sorted
    var s = tS(0)
    var m = s.size
    for (i <- 1 until n) {
      val c = compare(s, tS(i))
      s = tS(i)
      m += (s.size - c)
    }
    println(m)
  }

  def compare(s1: String, s2: String): Int = {
    val n = min(s1.size, s2.size)
    for (i <- 0 until n) {
      if (s1.charAt(i) != s2.charAt(i))
        return i
    }
    n
  }
}
