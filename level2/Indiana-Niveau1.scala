import scala.collection.mutable.HashMap

object Player extends App {

  // Définition d'un point de coordonnées
  class C(val x: Int, val y: Int) {
    private val _hash = (x, y).hashCode
    override def equals(e: Any): Boolean = e match {
      case c: C => x == c.x && y == c.y
      case _    => false
    }
    override def hashCode: Int = _hash
    override def toString: String = s"$x $y"

    def unary_-(): C = C(-x, -y)
    def unary_+(): C = C(x, y)

    def operate(c: C, f: (Int, Int) => Int): C = C(f(x, c.x), f(y, c.y))
    def +(c: C)  : C = operate(c, (e1, e2) => e1 + e2)
    def -(c: C)  : C = operate(c, (e1, e2) => e1 - e2)
    def cmp(c: C): C = operate(c, (e1, e2) => e1 compare e2)
    def :->(c: C): C = -(this cmp c)

    def out(w: Int = Int.MaxValue, h: Int = Int.MaxValue): Boolean = {
      x < 0 || y < 0 || x >= w || y >= h
    }
  }
  object C {
    def apply(x: Int, y: Int)        : C = new C(x, y)

    implicit def apply(e: (Int, Int)): C = C(e._1, e._2)
    implicit def apply(e: String)    : C = {
      val Array(x, y) = e.split(" ").map(_.toInt)
      C(x, y)
    }

    val NONE : C = C(-1, -1)
    val TOP  : C = C(0, -1)
    val DOWN : C = C(0, 1)
    val LEFT : C = C(-1, 0)
    val RIGHT: C = C(1, 0)
  }

  // Map des directions
  val dir = Map[C, String](C.RIGHT -> "RIGHT", C.LEFT -> "LEFT", C.DOWN -> "DOWN", C.TOP -> "TOP")
  val from = dir.map(e => (e._2, e._1))
  
  // Définition des pièces
  //  - tpe    : Type de pièce
  //  - sortie : Direction de la sortie en fonction de l'entrée
   class Piece(val tpe: Int) {
    val sortie = _init

    private def _init: Map[C, C] = tpe match {
      case 1  => Map(C.TOP -> C.DOWN, C.LEFT -> C.DOWN, C.RIGHT -> C.DOWN)
      case 2  => Map(C.LEFT -> C.RIGHT, C.RIGHT -> C.LEFT)
      case 3  => Map(C.TOP -> C.DOWN)
      case 4  => Map(C.TOP -> C.LEFT, C.RIGHT -> C.DOWN)
      case 5  => Map(C.TOP -> C.RIGHT, C.LEFT -> C.DOWN)
      case 6  => Map(C.LEFT -> C.RIGHT, C.RIGHT -> C.LEFT)
      case 7  => Map(C.TOP -> C.DOWN, C.RIGHT -> C.DOWN)
      case 8  => Map(C.LEFT -> C.DOWN, C.RIGHT -> C.DOWN)
      case 9  => Map(C.TOP -> C.DOWN, C.LEFT -> C.DOWN)
      case 10 => Map(C.TOP -> C.LEFT)
      case 11 => Map(C.TOP -> C.RIGHT)
      case 12 => Map(C.RIGHT -> C.DOWN)
      case 13 => Map(C.LEFT -> C.DOWN)
      case _  => Map()
    }

    def next(from: C): C = if (sortie contains from) sortie(from) else C.NONE
  }
  
  val piece = for (i <- 0 to 13) yield new Piece(i)
 
  val Array(w, h) = readLine.split(" ").map(_.toInt)
  val souterrain = new HashMap[C, Piece]
  for (y <- 0 until h) {
    val p = readLine.split(" ").map(_.toInt)
    for (x <- 0 until w) souterrain(C(x, y)) = piece(p(x))
  }
  val ex = readInt
  
  while (true) {
    val Array(xi, yi, posi) = readLine.split(" ")
    val c: C = (xi.toInt, yi.toInt)
    val n = souterrain(c).next(from(posi))
    println(c + n)
  }
} 
