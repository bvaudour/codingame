package super_computer

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution

import math._
import scala.util._

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val n = readInt
    val p = new Array[(Int, Int)](n)
    for (i <- 0 until n) {
      val s = readLine.split(" ").map(_.toInt)
      p(i) = (s(0), s(0) + s(1) - 1)
    }
    // Calcul nombre de calculs max
    val l = p.sortWith((e1, e2) => e1._2 < e2._2)
    val s = l.size
    var c = 1
    var i = 1
    var e = l(0)._2
    while (i < s) {
      while (i < s && l(i)._1 <= e) i += 1
      if (i < s) {
        c += 1
        e = l(i)._2
      }
    }
    println(c)
  }
}
