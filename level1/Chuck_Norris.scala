package chuck_norris

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._

object Solution {
  def main(args: Array[String]) {
    // Lecture séquence à coder
    val seq = readLine().toCharArray
    // Seq en ASCII
    val asc = new Array[Boolean](seq.size * 7)
    var idx = asc.size - 1
    for (c <- seq.reverse) {
      var div = c.toInt
      for (i <- 1 to 7) {
        asc(idx) = (div % 2 == 1)
        div /= 2
        idx -= 1
      }
    }
    // Byte courant
    var b = false
    // nb de bytes identiques consécutifs
    var id = 0
    // impression
    for (bC <- asc) {
      if (bC != b) {
        if (id != 0) {
          print(if (b) "0 " else "00 ")
          while (id > 0) {
            print('0')
            id -= 1
          }
          print(' ')
        }
        b = bC
        id = 1
      } else {
        id += 1
      }
    }
    if (id == 0) {
      println
    } else {
      print(if (b) "0 " else "00 ")
      while (id > 0) {
        print('0')
        id -= 1
      }
      println
    }
  }
}
