package roller_coaster

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val t = readLine.split(" ").map((e) => e.toInt)
    val (l, c, n) = (t(0), t(1), t(2))
    val file = new Array[Int](n)
    for (i <- 0 until n) file(i) = readInt
    // Calcul recette
    var r: Long = 0
    var idx = 0
    for (i <- 1 to c) {
      var (a, j) = (0, 0)
      var b = true
      while (b && j < n) {
        val v = file(idx)
        b = v <= (l - a)
        if (b) {
          a += v
          idx += 1
          if (idx == n) idx = 0
          j += 1
        }
      }
      r += a
    }
    println(r)
  }
}
