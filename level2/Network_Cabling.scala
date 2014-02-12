package network

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val n = readInt
    val (tX, tY) = (new Array[Int](n) , new Array[Int](n))
    var (min, max) = (Int.MaxValue, Int.MinValue)
    for (i <- 0 until n) {
      val s = readLine().split(" ").map((v) => v.toInt)
      if (s(0) < min) min = s(0)
      if (s(0) > max) max = s(0)
      tX(i) = s(0)
      tY(i) = s(1)
    }
    val l = max - min
    // Calcul médiane
    val tS = tY.sorted
    val med = (tS((n - 1) / 2), tS(n / 2))
    // Problème dépassement grosses valeurs => on convertit en long
    var (sum1, sum2) = (l.toLong, l.toLong)
    for (y <- tY) {
      sum1 += abs(y - med._1)
      sum2 += abs(y - med._2)
    }
    println(math.min(sum1, sum2))
  }
}
