package stock_exchange

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val n = readInt
    val tab = readLine.split(" ").map((e) => e.toInt)
    // Calcul de la perte la plus importante
    var (min, max) = (tab(0), tab(0))
    var p = 0
    for (i <- 1 until n) {
      if (tab(i) > max)
        max = tab(i)
      if (tab(i) < min) {
        min = tab(i)
        p = min - max
      }
    }
    println(p)
  }
}
