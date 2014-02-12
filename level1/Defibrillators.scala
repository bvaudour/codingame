package defibrillator

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._

object Solution {
  def main(args: Array[String]) {
    // Lecture Coordonnées
    val coordT = (toDouble(readLine), toDouble(readLine))
    // Lecture nombre de défibrillateurs
    val n = readInt
    // Lecture infos défibrillateurs + impression
    var d0 = Double.MaxValue
    var id = ""
    for (i <- 1 to n) {
      val info = readLine.split(";")
      val coordD = (toDouble(info(4)), toDouble(info(5)))
      val x = (coordT._1 - coordD._1) * cos((coordT._2 + coordD._2) / 2.)
      val y = coordT._2 - coordD._2
      val d = x * x + y * y
      if (d < d0) {
        d0 = d
        id = info(1)
      }
    }
    println(id)
  }
  def toDouble(s: String): Double = s.replace(',', '.').toDouble
}
