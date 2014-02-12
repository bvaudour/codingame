package ascii_art

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._

object Solution {
  def main(args: Array[String]) {
    val s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"
    // Lecture des dimensions des lettres
    val (l, h) = (readInt, readInt)
    // Lecture de la séquence
    val seq = readLine().toUpperCase
    // Lecture de la représentation ASCII
    val asc = new Array[String](h)
    for(i <- 1 to h)
      asc(i - 1) = readLine()
    // Impression
    for (l0 <- 0 until h) {
      for (c: Char <- seq) {
        var i = s.indexOf(c)
        if (i < 0)
          i = 26
        print(asc(l0).substring(i * l, (i + 1) * l))
      }
      println
    }
  }
}
