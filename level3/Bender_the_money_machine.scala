package money_machine

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.util._
import scala.collection.mutable.{HashMap => Map, HashSet => Set, MutableList => List, Stack}

object Solution {

  class Room(val id: String, val next1: String = "", val next2: String = "", val amount: Long = 0, var cumul: Long = 0) {
    def update(p: Room): Boolean = {
      val a = p.cumul + amount
      if (cumul >= a) return false
      cumul = a
      id != "E"
    }
  }

  def main(args: Array[String]) {

    //Récupération des données
    val n = readLine.toInt
    val map = new Map[String, Room]

    // Première salle
    var r = getRoom
    map.put(r.id, r)
    val first = r.id

    // Lignes suivantes
    for (i <- 1 until n) {
      r = getRoom
      map.put(r.id, r)
    }

    // Ajout sortie
    map.put("E", new Room("E"))

    //Calcul des cumuls
    val p = new Stack[Room]
    p.push(map(first))

    while (!p.isEmpty) {
      r = p.pop
      val (next1, next2) = (map(r.next1), map(r.next2))
      if (next1.update(r)) p.push(next1)
      if (next2.update(r)) p.push(next2)
    }

    println(map("E").cumul)

  }

  def getRoom: Room = {
    val l = readLine.split(" ")
    val a = l(1).toInt
    new Room(l(0), l(2), l(3), a, a)
  }

}
