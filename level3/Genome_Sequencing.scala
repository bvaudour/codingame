package genome

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.collection.mutable.{HashMap => Map, HashSet => Set, MutableList => List, Stack}

object Solution {

  def main(args: Array[String]) {
    // Récupération des données
    val n = readInt
    val sseq = new Array[Array[Char]](n)
    for (i <- 0 until n) sseq(i) = readLine.toCharArray
    // Permutations
    val perm = new List[Array[Int]]
    permutations(perm, "", "01234".substring(0, n))
    var min = -1
    for (p <- perm) {
      val s = concatPerm(p, sseq)
      if (min < 0 || min > s) min = s
    }
    println(min)
  }

  def concatPerm(p: Array[Int], seqs: Array[Array[Char]]): Int = {
    var conc = seqs(p(0))
    for (i <- 1 until p.size) conc = concatSeq(conc, seqs(p(i)))
    conc.size
  }

  def concatSeq(s1: Array[Char], s2: Array[Char]): Array[Char] = {
    var (b, i) = (false, 0)
    while (!b && i < s1.size) {
      b = compare(s1, s2, i)
      if (!b) i += 1
    }
    val out = new Array[Char](max(i + s2.size, s1.size))
    for (j <- 0 until out.size) {
      out(j) = if (j < s1.size) s1(j) else s2(j - i)
    }
    out
  }

  def compare(s1: Array[Char], s2: Array[Char], i: Int): Boolean = {
    val n = min(s1.size - i, s2.size)
    for (j <- 0 until n) {
      if (s1(i + j) != s2(j)) return false
    }
    true
  }

  def permutations(acc: List[Array[Int]], prefix: String, str: String) {
    val n = str.size
    if (n == 0) {
      val p = new Array[Int](prefix.size)
      for (i <- 0 until p.size) {
        p(i) = prefix.charAt(i) - '0'
      }
      acc += p
    }
    else {
      for (i <- 0 until n) permutations(acc, prefix + str.charAt(i), str.substring(0, i) + str.substring(i + 1, n))
    }

  }

}
