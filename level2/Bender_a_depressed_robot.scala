package depress_bender

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.util._
import scala.collection.mutable.{HashMap => Map, HashSet => Set, MutableList => List, Stack}

object Solution {

  class Position(val x: Int, val y: Int) {
    def ==(p: Position)  : Boolean = ==((p.x, p.y))
    def ==(p: (Int, Int)): Boolean = x == p._1 && y == p._2
    def !=(p: Position)  : Boolean = !(==(p))
    def !=(p: (Int, Int)): Boolean = !(==(p))
    def get(c: Char): Position = c match {
      case 'N' => new Position(x, y - 1)
      case 'S' => new Position(x, y + 1)
      case 'W' => new Position(x - 1, y)
      case _   => new Position(x + 1, y)
    }
  }

  class State(var direction: Char, var obstacles: Int, var inverseur: Boolean = false, var biere: Boolean = false) {
    def copy: State = new State(direction, obstacles, inverseur, biere)
    def ==(s: State): Boolean = direction == s.direction && obstacles == s.obstacles && inverseur == s.inverseur && biere == s.biere
  }

  class Jeu(val pos: Position, var tpe: Char, var state: Option[State] = None) {
    def stateChanged(s: State): Boolean = state match {
      case Some(st) => !(st == s)
      case None     => true
    }
  }

  def main(args: Array[String]) {

    //Récupération des données
    val dim = readLine.split(" ").map(_.toInt)
    val map = new Array[Array[Jeu]](dim(0))
    val nPos = new Position(-1, -1)
    var (begin, end, t1, t2) = (nPos, nPos, nPos, nPos)
    var nbObstacles = 0
    for (y <- 0 until dim(0)) {
      val s = readLine.toCharArray
      map(y) = new Array[Jeu](dim(1))
      for (x <- 0 until dim(1)) {
        val p = new Position(x, y)
        s(x) match {
          case '@' => begin = p
          case '$' => end   = p
          case 'T' => if (t1 == nPos) t1 = p else t2 = p
          case 'X' => nbObstacles += 1
          case _   => {}
        }
        map(y)(x) = new Jeu(p, s(x))
      }
    }

    // Calcul du parcours
    val parcours = new List[String]
    var p = begin
    val s = new State('S', nbObstacles)
    while (p != end) {
      val jC = map(p.y)(p.x)
      jC.tpe match {
        case 'T' => p = if (p == t1) t2 else t1
        case 'N' => s.direction = 'N'
        case 'S' => s.direction = 'S'
        case 'W' => s.direction = 'W'
        case 'E' => s.direction = 'E'
        case 'B' => s.biere     = !s.biere
        case 'I' => s.inverseur = !s.inverseur
        case _   => {}
      }
      var pNext = p.get(s.direction)
      if (s.biere && map(pNext.y)(pNext.x).tpe == 'X') {
        map(pNext.y)(pNext.x).tpe = ' '
        s.obstacles -= 1
      }
      if (map(pNext.y)(pNext.x).tpe == '#' || map(pNext.y)(pNext.x).tpe == 'X') {
        s.direction = if (s.inverseur) 'W' else 'S'
        pNext = p.get(s.direction)
        while (map(pNext.y)(pNext.x).tpe == '#' || map(pNext.y)(pNext.x).tpe == 'X') {
          if (s.biere && map(pNext.y)(pNext.x).tpe == 'X') {
            map(pNext.y)(pNext.x).tpe = ' '
          } else {
            pNext = p.get(changeDirection(s))
          }
        }
      }
      if (!jC.stateChanged(s)) {
        println("LOOP")
        return
      }
      jC.state = Some(s.copy)
      p = pNext
      parcours += nameDirection(s)
    }

    // Impression chemin
    parcours.foreach((e) => println(e))
  }

  def changeDirection(s: State): Char = {
    val d = s.direction match {
      case 'S' => if (s.inverseur) 'W' else 'E'
      case 'E' => if (s.inverseur) 'S' else 'N'
      case 'N' => if (s.inverseur) 'E' else 'W'
      case _   => if (s.inverseur) 'N' else 'S'
    }
    s.direction = d
    d
  }

  def nameDirection(s: State): String = {
    s.direction match {
      case 'S' => "SOUTH"
      case 'N' => "NORTH"
      case 'E' => "EAST"
      case _   => "WEST"
    }
  }

}
