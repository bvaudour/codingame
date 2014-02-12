package temperatures

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.util._

object Solution {
  def main(args: Array[String]) {
    // Lecture nombre d'éléments
    val n = readInt
    if (n == 0) {
      println(0)
      return
    }
    // Lecture températures
    val t = readLine().split(" ").map((v) => Integer.valueOf(v))
    //On ne tient pas compte du signe
    val t0 = t.map((v) => abs(v)).sorted
    // Température positive en priorité
    println(if (t.contains(t0(0))) t0(0) else -t0(0))
  }
}
