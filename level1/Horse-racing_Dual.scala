package horse_racing

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.util._

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val n = readInt
    val tP = new Array[Int](n)
    for (i <- 0 until n)
      tP(i) = readInt
    //Tri et calcul du plus petit écart
    val tS = tP.sorted
    var e = Int.MaxValue
    for (i <- 0 until n - 1) {
      val eC = abs(tS(i) - tS(i + 1))
      if (eC < e) e = eC
    }
    println(e)
  }
}
