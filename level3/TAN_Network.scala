package tan_network

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.collection.mutable.{HashMap => Map, HashSet => Set, MutableList => List, Stack}

object Solution {

  var (depart, arrivee): (Arret, Arret) = (null, null)
  val graphe = new Map[String, Arret]

  class Arret(val name: String, val lat: Double, val lon: Double) {

    val liaisons = new Set[Arret]
    var distance:Double = -1
    var (prev, next): (Arret, Arret) = (null, null)

    def add(e: Arret) = liaisons += e
    def cost(a: Arret): Double = {
      val x = (lon - a.lon) * cos((lat + a.lat) / 2.)
      val y = lat - a.lat
      x * x + y * y
    }
  }

  def main(args: Array[String]) {
    // Récupération des données
    constuctGraph

    // Si départ = arrivée, on arrête là
    if (depart == arrivee) { println(depart.name); return }

    // Maj des distance au départ
    computeDistances

    // Affichage de l'itinéraire
    printPath
  }

  def constuctGraph {
    val strip = (s: String) => s.substring(1, s.size - 1)
    val dbl = (s: String) => toRadians(s.toDouble)

    val (first, last) = (readLine, readLine)

    // Lecture des arrêts
    var n = readInt
    while (n > 0) {
      val l = readLine.split(",")
      graphe.put(l(0), new Arret(strip(l(1)), dbl(l(3)), dbl(l(4))))
      n -= 1
    }
    depart = graphe(first)
    arrivee = graphe(last)

    // Lecture des liaisons
    var m = readInt
    while (m > 0) {
      val l = readLine.split(" ")
      graphe(l(0)).add(graphe(l(1)))
      m -= 1
    }
  }

  def computeDistances {
    depart.distance = 0
    val pile = new Stack[Arret]
    pile.push(depart)
    while (!pile.isEmpty) {
      val a = pile.pop
      a.liaisons.foreach((n) => majDistance(a, n, pile))
    }
  }

  def majDistance(a: Arret, n: Arret, pile: Stack[Arret]) {
    if (a.prev == n) return  // Évite les boucles de routages
    val d = a.distance + a.cost(n)
    if (n.distance >= 0 && n.distance <= d) return // pas de changement
    n.distance = d
    n.prev = a
    // Si ce n'est pas le noeud d'arrivée, il est encore à traiter (si distance annoncée plus courte que distance connue)
    if (n != arrivee && (arrivee.distance < 0 || arrivee.distance > d)) pile.push(n)
  }

  def printPath {
    // Chemin non trouvé => distance infinie
    if (arrivee.distance < 0) { println("IMPOSSIBLE"); return }
    // On remonte le chemin pour pouvoir le tracer
    var a = arrivee
    while (a != depart) {
      val p = a.prev
      p.next = a
      a = p
    }
    // Traçage
    do {
      println(a.name)
      a = a.next
    } while (a != null)
  }

}
