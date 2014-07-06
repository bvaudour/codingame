import scala.collection.mutable.{HashMap, HashSet, Queue}

object Player extends App {

  // Classe représentant la liste des nœuds du réseau
  //   - id         : N° du nœud
  //   - liens      : Liste des nœuds directement rattachés
  //   - passerelle : Vrai si le nœud en question est une passerelle
  class Noeud(val id: Int, val liens: HashSet[Noeud] = new HashSet[Noeud], var passerelle: Boolean = false) {

    // Ajoute un lien entre deux nœuds
    def link(n: Noeud) {
      liens   += n
      n.liens += this
    }

    // Supprime le lien entre deux nœuds
    def unlink(n: Noeud): String = {
      liens   -= n
      n.liens -= this
      s"${n.id} ${id}"
    }

    // Informations sur les voisins
    //   - countL : Nombre de voisins
    //   - countP : Nombre de voisins qui sont des passerelles
    //   - orphan : Vrai s'il s'agit d'une passerelle complètement isolée
    def countL: Int     = liens.size
    def countP: Int     = liens.count(_.passerelle)
    def orphan: Boolean = liens.isEmpty && passerelle

    // Liste des voisins de type "passerelle"
    def passerelles: HashSet[Noeud] = liens.filter(_.passerelle)
  }

  // Classe décrivant les informations nécessaires à l'analyse
  //   - id       : Nœeud concerné par l'analyse
  //   - distance : Distance du nœud par rapport à la position actuelle de Skynet
  //   - parent   : Nœud menant à la position de Skynet
  class Analyse(val id: Noeud, var distance: Int = Int.MaxValue, var parent: Option[Noeud] = None) {

    // Définit le nœud précédent
    def setParent(a: Analyse): Analyse = {
      parent   = Some(a.id)
      distance = a.distance + 1
      this
    }

    // Nœud précédent
    def prev: Noeud = parent.get
  }

  // Lit une entrée de ligne sous forme de tableau d'entiers
  def readArray: Array[Int] = readLine.split(" ").map(_.toInt)

  // Récupère les distances de chaque nœud par rapport à la position de Skynet
  def getDistances(network: Seq[Noeud], skynet: Noeud): HashMap[Noeud, Analyse] = {
    val out  = new HashMap[Noeud, Analyse]
    val file = Queue[Noeud](skynet)
    out(skynet) = new Analyse(skynet, 0)
    while (!file.isEmpty) {
      val p = file.dequeue
      for (l <- p.liens; if (!out.contains(l))) {
        out(l) = new Analyse(l).setParent(out(p))
        file += l
      }
    }
    out
  }

  // Recherche le lien à couper en priorité
  def mostDangerousLink(distances: HashMap[Noeud, Analyse], gateways: HashSet[Noeud]): (Noeud, Noeud) = {
    val g = gateways.minBy(n => distances(n).distance)
    (distances(g).prev, g)
  }

  // Lecture des paramètres
  val Array(n, l, e) = readArray
  val network        = for (i <- 0 until n) yield new Noeud(i)
  for (_ <- 0 until l) {
    val Array(n1, n2) = readArray
    network(n1) link network(n2)
  }
  val gateways       = new HashSet[Noeud]
  for (_ <- 0 until e) {
    val p = network(readInt)
    p.passerelle = true
    gateways    += p
  }

  // Exécution
  while (true) {
    // Recherche et impression du lien à couper
    val (n1, n2) = mostDangerousLink(getDistances(network, network(readInt)), gateways)
    println(n1 unlink n2)

    // Si la passerelle concernée par la coupure est complètement isolée, il n'y a plus de raison de la garder dans la liste des passerelles
    if (n1.orphan) gateways -= n1
    if (n2.orphan) gateways -= n2
  }
}
