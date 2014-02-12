import math._
import scala.util._
import scala.language.implicitConversions
import scala.collection.mutable.{HashMap => Map, HashSet => Set, Stack }

object Solution {

  // Pour des raisons pratiques, on redéfinit le tuple
  type I2   = (Int, Int)
  //type C2   = (Coord, Coord)
  type B2I  = Boolean => Int
  type I2B  = Int     => Boolean

  // Fonctions utiles
  val inRange: (Int, Int) => I2B = (from, to) => e => e >= from && e <= to

  // Coordonnées
  //  - c : N° de colonne
  //  - l : N° de ligne
  class Coord(val c: Int, val l: Int) {
    def tuple            : I2      = (c, l)
    def isNull           : Boolean = c < 0 || l < 0
    def ==(e: Coord)     : Boolean = c == e.c && l == e.l
    def !=(e: Coord)     : Boolean = !(this == e)
    override def toString: String  = "[" + c + "," + l + "]"
  }
 
  // Permet les conversions implicites
  object Coord {
    val none                      : Coord = Coord(-1, -1)
    def apply()                   : Coord = Coord(0, 0)
    implicit def toTuple(c: Coord): I2    = c.tuple
    implicit def apply(v: I2)     : Coord = new Coord(v._1, v._2)
  }

  // Dimensions de l'image
  //  - w : Largeur
  //  - h : Hauteur
  //  - s : Nombre de pixels de l'image
  class Dim(val w: Int, val h: Int) {
    val s = w * h

    def this(a: Array[Int]) = this(a(0), a(1))
    def this(s: String)     = this(s.split(" ").map(_.toInt))

    // Fonctions de conversions : de la forme x2y où x et y peuvent prendre les valeurs suivantes :
    //  - h -> numéro de pixel en parcours horizontal
    //  - v -> numéro de pixel en parcours vertical
    //  - c -> coordonnées sous la forme (n° colonne, n° ligne)
    def h2c(p: Int)  : Coord = (p % w, p / w)
    def v2c(p: Int)  : Coord = (p / h, p % h)
    def c2h(c: Coord): Int   = c.c + c.l * w
    def c2v(c: Coord): Int   = c.l + c.c * h
    def h2v(p: Int)  : Int   = c2v(h2c(p))
    def v2h(p: Int)  : Int   = c2h(v2c(p))

    // Inutile ! Sert uniquement en phase de test
    override def toString: String = (w, h).toString
  }

  // Définition d'une rangée
  //  - c    : Colonne
  //  - from : Ligne de début
  //  - to   : Ligne de fin
  class Rangee(val c: Int, val from: Int, val to: Int) {
    def geF(r: Rangee)     : Boolean = from <= r.from
    def geT(r: Rangee)     : Boolean = to   >= r.to
    def eq(r: Rangee)      : Boolean = from == r.from && to == r.to
    def contains(r: Rangee): Boolean = geF(r) && geT(r)
    def isNull             : Boolean = from < 0 || to < 0
    def center             : Coord   = (c, (from + to) / 2) // Recherche le pixel central de la rangée

    // Toujours ces fichues phases de test...
    override def toString: String = c.toString + "->" + (from, to)
  }

  // Conversions implicites
  object Rangee {
    implicit def apply(e: (Int, Int, Int)): Rangee          = new Rangee(e._1, e._2, e._3)
    implicit def toTuple(e: Rangee)       : (Int, Int, Int) = (e.c, e.from, e.to)
  }

  // Ligne de portée
  //  - id   : N° de ligne (Par convention, la ligne en dessous de la portée, celle du do, est 0)
  //  - from : Début de ligne
  //  - to   : Fin de ligne
  class Ligne(val id: Int, val from: Int, val to: Int) {
    val contains: I2B = inRange(from, to)

    // Pour le débug...
    override def toString: String = "l" + id + "->" + (from, to)
  }

  // Notes de portée
  //  - id   : ID sous la forme "XY" où X identifie la note, et Y la position (L pour ligne, I pour interligne)
  //  - from : Valeur min de la ligne de la tête de note
  //  - to   : Valeur max de la ligne de la tête de note
  class Note(val id: String, val from: Int, val to: Int) {
    val contains         : I2B     = inRange(from, to)
    def value            : Char    = id.charAt(0)

    // Débug, également...
    override def toString: String  = id.toString + "->" + (from, to)
  }

  // Portée
  //  - cBL1 : Coordonnée de début de première ligne
  //  - cEL5 : Coordonnée de fin de dernière ligne
  //  - cBI1 : Coordonnée de début de la première interligne
  //  - CBL2 : Coordonnée de début de la deuxième ligne
  class Portee(cBL1: Coord, cEL5: Coord, cBI1: Coord, cBL2: Coord) {

    // Colonnes d'extrémités (Permet de déterminer la fenêtre d'analyse)
    val (from, to) = (cBL1.c + 1, cEL5.c - 1)
 
    // Épaisseur de la ligne & espace interligne
    val (hL, hI) = (cBI1.l - cBL1.l, cBL2.l - cBI1.l)
    
    // Récupération des propriétés des lignes de portée et de la position des notes
    val lignes: Map[Int, Ligne]   = _initL
    val notes : Map[String, Note] = _initN

    private def _initL: Map[Int, Ligne] = {
      val out    = new Map[Int, Ligne]
      var lC     = cBL1.l
      val (e, n) = (hL - 1, hL + hI)
      for (i <- 5 to 0 by -1) {
        out(i) = new Ligne(i, lC, lC + e)
        lC += n
      }
      out
    }

    private def _initN: Map[String, Note] = {
      val out = new Map[String, Note]
      val e2  = (hL + 1) / 2 // demi-épaisseur de la ligne
      val hNI = hI + 2 * e2  // Hauteur théorique maximale de la tête de note
      val i2  = (hI + 1) / 2 // demi-interligne
 
      // Resp. liste des ids des notes d'interligne et de ligne
      val kI = Seq("DI", "FI", "AI", "CI", "EI", "GI")
      val kL = Seq("CL", "EL", "GL", "BL", "DL", "FL")

      // Fonctions de calcul des notes (interligne et ligne)
      def cI(id: String, l: Ligne): Note = {
        val t = l.from + e2 - 1
        val f = t - hNI + 1
        new Note(id, f, t)
      }
      def cL(id: String, l: Ligne): Note = {
        val f = l.from - i2 - 1
        val t = l.to   + i2 + 1
        new Note(id, f, t)
      }

      // Remplissage...
      for (i <- 0 to 5) {
        val (idI, idL, l) = (kI(i), kL(i), lignes(i))
        out(idI) = cI(idI, l)
        out(idL) = cL(idL, l)
      }
      out
    }

    // Fonctions de contrôle
    //  - isLine  : le pixel donné est situé sur une ligne de portée
    //  - isHampe : les extrémités données correspondent à un range de hampe (À noter que l'analyse de la hampe n'est pas nécessaire au calcul. Il suffit de la considérer comme une colonne vide)
    def isLine(e: Int)             : Boolean = lignes.values.exists(_.contains(e))
    def isHampe(from: Int, to: Int): Boolean = to - from > hI + 1
    def isHampe(r: Rangee)         : Boolean = isHampe(r.from, r.to)

    // Récupération de la liste des notes correspondant à une ligne
    def lNotes(e: Int)  : Stack[Note] = {
      val out = new Stack[Note]
      for (n <- notes.values) if (n.contains(e)) out.push(n)
      out
    }

    // Note d'une rangée
    def note(r: Rangee): Note = {
      var l = r.from
      var p = lNotes(l)
      while (l < r.to) {
        l += 1
        val pT = new Stack[Note]
        while (!p.isEmpty) {
          val n = p.pop
          if (n.contains(l)) pT.push(n)
        }
        p = pT
      }
      p.pop
      // Autre possibilité : Faire une intersection de pile de notes récupérée uniquement à partir des extrémités. C'est plus rapide, mais moins sûr...
    }

    // Pour les tests...
    def strLines: String = (0 to 5).map(id => lignes(id)).mkString("(", ";", ")")
    def strNotes: String = Seq("CL", "DI", "EL", "FI", "GL", "AI", "BL", "CI", "DL", "EI", "FL", "GI").map(id => notes(id)).mkString("(", ";", ")")
  }

  // Image partition
  //  - d : dimensions de l'image
  class Scan(val d: Dim) {
  
  def this(dim: String, dwe: String) = { this(new Dim(dim)); decode(dwe.split(" ")) }
  def this(w: Int, h: Int)           = this(new Dim(w, h))
  
  // Tableau représentant l'image de la partition
    // Les données sont insérées horizontalement (parcours ligne à ligne)
    // Valeur des pixels :
    //  - true  si pixel noir
    //  - false sinon
    val img = new Array[Boolean](d.s)

    // Convertit l'algorithme DWE en tableau de pixels représentant l'image
    def decode(dwe: Array[String]) {
      var p = 0
      for (i <- 0 until dwe.size by 2) {
        val isBlack = dwe(i) == "B"
        var n       = dwe(i + 1).toInt
        while (n > 0) {
          img(p) = isBlack
          n -= 1
          p += 1
        }
      }
    }

    // Getters et setters de l'image
    def apply(c: Coord)                         : Boolean = img(d.c2h(c))
    def update(c: Coord, isBlack: Boolean)      : Unit    = img(d.c2h(c)) = isBlack
    def update(c: Int, l: Int, isBlack: Boolean): Unit    = update((c, l), isBlack)

    // Récupération de tous les pixels d'une ligne ou d'une colonne donnée (Finalement inutilisé dans cet exercice...)
    def line(l: Int)  : Seq[Boolean] = { val b = l * d.w; img.slice(b, b + d.w) }
    def column(c: Int): Seq[Boolean] = (c until d.s by d.w).map(p => img(p))

    // Itérateur générique (parcours vertical)
    val iterator: Boolean => Int => Coord => Coord = isBlack => incr => first => {
      var c = first
      var p = d.c2v(c)
      while (this(c) != isBlack) {
        p += incr
        c = d.v2c(p)
      }
      c
    }
    // Itérateurs par couleur
    val (itB, itW) = (iterator(true), iterator(false))
    // Itérateurs par couleur et par sens de parcours
    val (nB, pB) = (itB(1), itB(-1))
    val (nW, pW) = (itW(1), itW(-1))
    
    def nBlack(c: Coord): Coord = nB(c)
    def pBlack(c: Coord): Coord = pB(c)
    def nWhite(c: Coord): Coord = nW(c)
    def pWhite(c: Coord): Coord = pW(c)

    // Recherche des caractéristiques de la portée
    def portee : Portee = {
      // Recherche du début et de la fin de la portée
      val (begin, end) = (nBlack(0, 0), pBlack(d.w - 1, d.h - 1))
      // Calcul de l'épaisseur de la portée
      val e1 = nWhite(begin)
      // Calcul de l'espace interligne
      val e2 = nBlack(e1)
      new Portee(begin, end, e1, e2)
    }

    // Valeur de la couleur -> temps de la note
    def value(c: Coord): Char = if (this(c)) 'Q' else 'H'

    // Fonctions spéciales pour générer l'ASCII Art !!! (Inutile, donc indispensable...)
    //  - mkstring : Transforme une ligne de la partition en succession de caractères ('@' pour les pixels noirs, '.' pour les blancs)
    //  - print    : Imprime les lignes ASCII ainsi générées
    //  - divBy2   : Permet de diviser la taille de l'image par deux (pratique quand le nombre de colonne en sortie est limité...)
    //                 -> Cela dit, le rendu n'est pas terrible. Il suffit tout juste pour identifier les notes.
    def mkstring(l : Int): String = line(l).map(b => if (b) "@" else ".").mkString
    def print            : Unit   = for (l <- 0 until d.h) println(mkstring(l))
    def divBy2           : Scan   = {
      // Principe :
      //  - On prend un carré de pixels (2 × 2) de l'image d'origine
      //  - S'il y a plus de 2 pixels noirs parmi les 4, on retient un pixel noir, sinon, ce sera un pixel blanc
      //  - Ce carré de pixel, avec la couleur calculée, deviendra 1 pixel de la nouvelle image
      def bl(c: Coord): Int = if (this(c)) 1 else 0
      val out = new Scan(d.w / 2, d.h / 2)
      for (c <- 0 until out.d.w) {
        for (l <- 0 until out.d.h) {
          val (c2, l2) = (c * 2, l * 2)
          val ct = bl(c2, l2) + bl(c2, l2 + 1) + bl(c2 + 1, l2) + bl(c2 + 1, l2 + 1)
          out(c, l) = if (ct >= 2) true else false
        }
      }
      out
    }
  }

  // Partition
  //  - sc : Représentation globale de la partition
  //  - pt : Caractéristiques de la portée
  class Partition(dim: String, dwe: String) {

    val sc = new Scan(dim, dwe)
    val pt = sc.portee

    // Fonctions de recherche et d'analyse
    //  - isNote  : le pixel donné correspond à un pixel de note bien identifié
    //  - hasNote : La colonne donnée contient au moins un pixel identifiable à une note
    //  - range   : Extrémités de la position de la note sur la colonne
    def isNote(c: Coord): Boolean = sc(c) && !pt.isLine(c.l)
    def hasNote(c: Int) : Boolean = (0 until sc.d.h).exists(l => isNote(c, l))
    def range(c: Int)   : Rangee  = {
      var (l0, l1) = (-1, -1)
      for (l <- 0 until sc.d.h) if (isNote(c, l)) { if (l0 < 0) l0 = l else l1 = l }
      if (l0 < 0) return (c, l0, l1)
      if (l1 < 0) l1 = sc.nBlack(c, l0 + 1).l
      (c, l0, l1)
    }

    // Recherche des notes
    def searchNotes: String = {

      // Principe :
      //  - On "lit" l'image colonne par colonne, depuis le début de la portée jusqu'à la fin de la portée
      //  - Dès que l'on détecte un début de note, on se place sur la colonne centrale de ladite note (en supposant que les notes sont bien rondes...)
      //  - On récupère, grâce à la position horizontale, le nom de la note (normalement unique...)
      //  - On récupère, grâce au pixel le plus au centre de cette rangée, la couleur de la note, et donc sa durée
      //  - On recherche la note suivante et on réitère...
      val out = new StringBuilder  // Permet de collecter les notes analysées
      val i2  = pt.hI / 2          // Pour trouver la colonne du milieu de la note
      var c = pt.from              // On commence par la colonne du début de portée (et on gagne, ainsi, quelques poullièmes de sec. en temps de traitement !)

      def update(r: Rangee) {
        if (out.length != 0) out += ' '
        out += (pt.note(r).value, sc.value(r.center))
      }
      
      def search() {
        val r = range(c)
        if (r.isNull) { c += 1; return }  // Colonne ne contenant pas de note
        c += i2                           // Passage direct à la colonne centrale
        update(range(c))                  // Màj du buffer pour la sortie
        c += i2                           // Passage direct à la fin de la note
        while (hasNote(c)) c += 1         // Si la note est plus large que prévu, on essaie d'aller un peu plus vite...
      }

      while (c <= pt.to) search()
      out.toString
    }

  }

  def main(args: Array[String]) {
    val p = new Partition(readLine, readLine)
    println(p.searchNotes)
  }
}
