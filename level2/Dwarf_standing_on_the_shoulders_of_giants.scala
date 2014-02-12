package dwarf

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object Solution {
  def main(args: Array[String]) {
    // Lecture des données
    val n = readInt
    val m = new HashMap[String, HashSet[String]]()
    for (i <- 1 to n) {
      val s = readLine.split(" ")
      if (!m.contains(s(0))) m.put(s(0), new HashSet[String])
      if (!m.contains(s(1))) m.put(s(1), new HashSet[String])
      m(s(0)) += s(1)
    }
    var s = 0
    for (k <- m.keySet) {
      val s0 = size(k, m)
      if (s0 > s) s = s0
    }
    println(s)
  }

  def size(k: String, m: HashMap[String, HashSet[String]]): Int = {
    if (m(k).size == 0) 1 else 1 + m(k).map((v) => size(v, m)).max
  }
}
