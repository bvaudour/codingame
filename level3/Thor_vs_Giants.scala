import math.{abs, max, min}

object Player extends App {

  // Classe générique pour tuple amélioré
  abstract class Vect(val x: Int, val y: Int) {

  // Hash conservé en cache (évite de le recalculer à chaque fois)
    private val _hash = (x, y).hashCode

    override def equals(o: Any): Boolean = o match {
      case c: Vect => x == c.x && y == c.y
      case _       => false
    }

    override def hashCode: Int = _hash
  }

  // Objet compagnon. Juste du sucre syntaxique pour pouvoir le convertir en tuple
  object Vect {
    implicit def toTuple(e: Vect): (Int, Int) = (e.x, e.y)
  }

  // Directions
  class D(x: Int, y: Int) extends Vect(x, y) {
    override def toString: String = {
      var out = y match {
        case -1 => "N"
        case 1  => "S"
        case 0  => ""
        case _  => return ""
      }
      x match {
        case -1 => out += "W"
        case 1  => out += "E"
        case 0  =>
        case _  => return ""
      }
      out
    }
  }

  object D {
    implicit def apply(e: (Int, Int)): D = new D(e._1, e._2)
  }

  // Coordonnées
  class C(x: Int, y: Int) extends Vect(x, y) {

    // Vérifie que la coordonnée est bien dans les limites de la carte
    def outOfMap: Boolean = x < 0 || y < 0 || x >= C.w || y >= C.h

    // Incrémente la coordonnée selon la direction choisie
    def +(d: Vect): C = (x + d.x, y + d.y)

    // Direction vers la coordonnée
    def directionTo(p: C): D = (p.x compare x, p.y compare y)

    // Distance à la coordonnée
    def distanceTo(p: C): Int = max(abs(p.x - x), abs(p.y - y))
  }

  object C {

    // Dimensions de la carte
    val (w, h) = (40, 18)
    implicit def apply(e: (Int, Int)) : C = new C(e._1, e._2)
  }

  // Lit l'entrée sous forme de tuple
  def input: (Int, Int) = {
    val Array(x, y) = readLine.split(" ").map(_.toInt)
    (x, y)
  }

  // Liste des directions possibles (y compris la position statique)
  val directions: Set[D] = (for (x <- -1 to 1; y <- -1 to 1) yield D(x,y)).toSet

  val outOfDirection: C => D => Boolean = c => d => (c + d).outOfMap

  // Filtre les directions possibles à partir d'un point sans sortir de la carte
  def availableDirections(c: C): Set[D] = directions.filterNot(outOfDirection(c))

  // Définit un range de coordonnées ne sortant pas de la carte
  val range = (eMax: Int) => (e0: Int, e1: Int) => max(0, e0) to min(e1, eMax)
  val (rX, rY) = (range(C.w - 1), range(C.h - 1))

  // Tour de jeu
  // la donnée t ne sert que pour le débug, elle est incrémentée à chaque tour de jeu
  // thor : coordonnées de Thor au début du tour
  class Tour(thor: C, val t: Int) {

    // Initialisation des variables :
    //  h : Nombre de coups de marteau encore autorisés
    //  n : Nombre de géants encore vivants
    //  min2kill : Nombre minimum de géants qu'il faut tuer d'un coup pour ne pas perdre la main
    //  giants : Liste des coordonnées des géants
    //  m : Coordonnées du centre de la carte
    //  g : Coordonnées du barycentre des géants
    val (h, n) = input
    val min2kill = if (n % h == 0) n / h else n / h + 1
    val giants: Set[C] = (for (i <- 0 until n) yield C(input)).toSet
    val (m, g) = (C(C.w / 2, C.h / 2), meanGiants)

    // Nombre de géants tuables pour ce tour
    def nbKillable: Int = {
      var out = 0
      for (x <- rX(thor.x - 4, thor.x + 4); y <- rY(thor.y - 4, thor.y + 4)) {
        if (giants contains C(x, y)) out += 1
      }
      out
    }

    // Barycentre des coordonnées des géants
    def meanGiants: C = {
      var out = C(0, 0)
      for (c <- giants) out = out + c
      C(out.x / n, out.y / n)
    }

    // Zone de confort (distance au géant le plus proche)
    def comfortZone(c: C): Int = giants.map(_.distanceTo(c)).min

    // On tue
    def kill: C = {
      println("STRIKE")
      thor
    }

    // On attend
    def noMove: C = {
      println("WAIT")
      thor
    }

    // On se dirige vers la direction d
    def dirTo(d: D): C = {
      if (d == D(0, 0)) {
        return if (isDangerous(thor)) kill else noMove
      }
      println(d)
      thor + d
    }

    // On se dirige vers le barycentre
    def dir2mean: C = dirTo(thor.directionTo(g))

    // On cherche la meilleure direction
    // Principe :
    //  1. On recherche la direction qui nous ramène au plus près des géants sans mettre Thor en danger
    //  2. Si plusieurs directions sont possibles, on cherche cherche qui nous rapproche du Barycentre ou bien du centre de la carte (partie à améliorer : marche pour les données de test mais pas pour toutes les données d'analyse (échec dans le cas : "20 géants - 2 coups"))
    def bestDir: D = {
      var d: D = (0, 0)
      var (danger, d2c, d2g) = (Int.MaxValue, Int.MaxValue, Int.MaxValue)
      for (e <- availableDirections(thor)) {
        val c = thor + e
        val (dgr, d2c0, d2g0) = (comfortZone(c), c.distanceTo(m), c.distanceTo(g))
        if (dgr < 2 || dgr > danger) {
        } else if (dgr < danger) {
          d      = e
          danger = dgr
          d2c    = d2c0
          d2g    = d2g0
        } else if (d2g0 < d2g || d2c0 < d2c) {
          d   = e
          d2c = d2c0
          d2g = d2g0
        }
      }
      d
    }

    // On est en zone de danger
    def isDangerous(c: C): Boolean = {
      for (e <- giants) {
        if (c.distanceTo(e) < 2) return true
      }
      false
    }

    // Exécute le tour et sort la nouvelle coordonnée de Thor
    def exec: C = {
      val k = nbKillable

      // Si on peut tuer plus de géants que le minimum autorisé, on les tue
      if (k >= min2kill) return kill

      // S'il n'y a aucun méchant alentour, on se dirige vers le barycentre
      if (k == 0) return dir2mean

      // Autres cas
      dirTo(bestDir)
    }
  }

  // Jeu proprement dit
  var thor = C(input)
  var i = 1
  while (true) {
    thor = new Tour(thor, i).exec
    i += 1
  }

}
