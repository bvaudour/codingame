package conway_sequence

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.collection.mutable.MutableList

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val r = readInt
    val l = readInt
    // Calculs jusqu'à la ligne l
    var l0 = Array(r)
    for (i <- 1 until l) {
      val lst = new MutableList[Int]()
      var (c, n) = (1, l0(0))
      for (j <- 1 until l0.size) {
        if (l0(j) != n) {
          lst += (c, n)
          c = 0
          n = l0(j)
        }
        c += 1
      }
      lst += (c, n)
      l0 = lst.toArray
    }
    println(l0.mkString(" "))
  }
}
