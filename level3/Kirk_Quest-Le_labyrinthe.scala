import scala.collection.mutable.{HashMap, HashSet, Queue}

object Player extends App {

  // Coordonnées
  class C(val l: Int, val c: Int) {
    private val _hash = (l, c).hashCode
    override def equals(e: Any): Boolean = e match {
      case o: C => o.l == l && o.c == c
      case _    => false
    }
    // On utilise le hash car nécessaire pour l'utiliser dans une hashmap
    
    // Additionne / soustrait deux coordonnées/directions
    override def hashCode: Int = _hash
    def +(e: C): C = C(l + e.l, c + e.c)
    def -(e: C): C = C(l - e.l, c - e.c)
    // Vérifie que la coordonnée n'est pas hors d'un champ délimité
    def valid(h: Int, w: Int): Boolean = l >= 0 && l < h && c >= 0 && c < w
    // Voisins de la coordonnée
    def neighbors                : Seq[C] = directions.map(_ + this)
    // Voisins valides de la coordonnée
    def neighbors(h: Int, w: Int): Seq[C] = neighbors.filter(_.valid(h, w))
  }
  object C {
    implicit def apply(e: (Int, Int)): C = new C(e._1, e._2)
    implicit def apply(e: String)    : C = {
      val Array(l, c) = e.split(" ").map(_.toInt)
      (l, c)
    }
    val ZERO  = C( 0,  0)
    val NONE  = C(-1, -1)
    val UP    = C(-1,  0)
    val DOWN  = C( 1,  0)
    val LEFT  = C( 0, -1)
    val RIGHT = C( 0,  1)
  }
  
  // Liste des directions possibles
  val directions = Seq[C](C.UP, C.LEFT, C.DOWN, C.RIGHT)

  val direction = Map[C, String](C.RIGHT -> "RIGHT", C.LEFT -> "LEFT", C.DOWN -> "DOWN", C.UP -> "UP")

  // Description du labyrinthe
  //  - w        : largeur du labyrinthe
  //  - h        : hauteur du labyrinthe
  //  - position : position actuelle de Kirk
  //  - depart   : position de départ de Kirk
  //  - arrivee  : position du panneau de commande
  class Labyrinthe(val w: Int, val h: Int) {
    private val _l        = new HashMap[C, Char]
    var position          = C.NONE
    var (depart, arrivee) = (C.NONE, C.NONE)

    // Lit le type de case à une position donnée
    def apply(c: C): Char = _l(c)
    // Modifie le type de case à une position donnée
    def update(c: C, e: Char): Unit = _l(c) = e

    // Vérifie que la position du panneau de commande est connue
    def arrivalKnown: Boolean = arrivee != C.NONE
    
    // Met à jour la position et la connaissance du labyrinthe
    def update() {
      position = readLine
      for (l <- 0 until h) {
        val r = readLine.toCharArray
        for (c <- 0 until w) {
          update(C(l, c), r(c))
          r(c) match {
            case 'T' => depart  = C(l, c)
            case 'C' => arrivee = C(l, c)
            case _   => {}
          }
        }
      }
    }
    
    // Voisins valides d'une position donnée
    def neighbors(c: C): Seq[C] = c.neighbors(h, w).filterNot(e => this(e) == '#')

    // Recherche la position inconnue dans le labyrinthe la plus proche de la position actuelle
    def explore: C = {
      val file   = new Queue[C]
      val traite = new HashSet[C]
      file += position
      while (!file.isEmpty) {
        val p = file.dequeue
        traite += p
        for (n <- neighbors(p); if (!traite.contains(n))) {
          if (this(n) == '?') return n
          file += n
        }
      }
      C.NONE
    }
    
    // Recherche la prochaine position à aller
    def next(e: C): C = {
      val path     = new HashMap[C, C]
      val distance = new HashMap[C, Int]
      val file     = new Queue[C]
      file += position
      distance(position) = 0
      while (!file.isEmpty) {
        val p = file.dequeue
        val d = distance(p) + 1
        for (n <- neighbors(p); if (!distance.contains(n) || distance(n) > d)) {
          distance(n) = d
          path(n)     = p
          file += n
        }
      }
      var out = e
      while (path(out) != position) out = path(out)
      out
    }
    override def toString: String = {
      var out = new StringBuilder
      for (l <- 0 until h) {
        for (c <- 0 until w) out += this((l, c))
        out += '\n'
      }
      out.toString
    }
  }
  
  // Données d'entrée (dans le cas présent, l'alarme n'est pas utilisée, mais pour affiner le jeu, on pourrait…)
  val Array(h, w, a) = readLine.split(" ").map(_.toInt)
  val l = new Labyrinthe(w, h)
  
  // Flag mode retour enclenché
  var ret = false

  // Jeu proprement dit
  while (true) {
    l.update()
    if (l(l.position) == 'C') ret = true
    val g = if (ret) l.depart else if (l.arrivalKnown) l.arrivee else l.explore
    val n = l.next(g)
    println(direction(n - l.position))
  }
}
