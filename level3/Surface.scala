package surface

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.collection.mutable.{HashMap => Map, HashSet => Set, MutableList => List, Stack}

object Solution {

  class Coord(val x: Int, val y: Int) {
    def neighbor(c: Coord): Boolean = {
      if (x == c.x) return abs(y - c.y) == 1
      if (y == c.y) return abs(x - c.x) == 1
      false
    }
    def ==(c: Coord): Boolean = x == c.x && y == c.y
  }

  class Lac(private val coords: Set[Coord] = new Set[Coord]) {
    def this(c: Coord) = { this(); this += c}
    def this(l: Lac) = {this; this += l }
    def surface: Int = coords.size
    def isEmpty: Boolean = surface == 0
    def +=(c: Coord): Unit = coords += c
    def +=(l: Lac): Unit = coords ++= l.coords
    def contains(c: Coord): Boolean = {
      for (e <- coords) {
        if (e == c) return true
      }
      false
    }
    def neighbor(c: Coord): Boolean = {
      for (e <- coords) {
        if (e.neighbor(c)) return true
      }
      false
    }
    def neighbor(l: Lac): Boolean = {
      for (e <- coords) {
        if (l.neighbor(e)) return true
      }
      false
    }

  }

  val lacs = new List[Lac]

  def main(args: Array[String]) {
    // Récupération des données
    val (l, h) = (readInt, readInt)
    var pile = new Stack[Lac]
    var lac: Lac = new Lac
    for (y <- 0 until h) {
      val s = readLine.toCharArray
      for (x <- 0 until l) {
        if (s(x) == 'O') {
          val c = new Coord(x, y)
          if (lac.isEmpty || lac.neighbor(c)) {
            lac += c
          } else {
            pile.push(lac)
            lac = new Lac(c)
          }
        }
      }
    }
    if (!lac.isEmpty) pile.push(lac)
    while (!pile.isEmpty) {
      lac = pile.pop
      var b = false
      do {
        b = false
        val pTmp = new Stack[Lac]
        for (e <- pile) {
          if (lac.neighbor(e)) {
            b = true
            lac += e
          } else pTmp.push(e)
        }
        pile = pTmp
      } while (b)
      lacs += lac
    }
    // Récupération des coordonnées à tester
    val n = readInt
    for (i <- 0 until n) {
      val s = readLine.split(" ")
      val c = new Coord(s(0).toInt, s(1).toInt)
      println(surface(c))
    }

  }

  def surface(c: Coord): Int = {
    for (l <- lacs) if (l.contains(c)) return l.surface
    0
  }

}
