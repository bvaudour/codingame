object Player extends App {

  // Nombre de montagnes
  val nm = 8

  // Sorties possibles
  def fire(): Unit = println("FIRE")
  def hold(): Unit = println("HOLD")

  while (true) {
    // Lecture des données d'entrée
    val Array(sx, sy) = readLine.split(" ").map(_.toInt)
    val mh = for (i <- 0 until nm) yield readLine.toInt

    // Recherche de la montagne la plus haute
    var (m, h) = (-1, 0)
    for (i <- 0 until nm) {
      if (mh(i) > h) {
        m = i
        h = mh(i)
      }
    }

    // Si on est juste au-dessus de la montagne la plus haute, on tire, sinon, on attend
    if (sx == m) fire() else hold()
  }

}
