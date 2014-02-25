object Player extends App {

    // Liste des directions possibles, indexées par l'incrément
    val directions = Map[(Int, Int), String](
        (0, -1)  -> "N" ,
        (1, -1)  -> "NE",
        (1, 0)   -> "E" ,
        (1, 1)   -> "SE",
        (0, 1)   -> "S" ,
        (-1, 1)  -> "SW",
        (-1, 0)  -> "W" ,
        (-1, -1) -> "NW"
    )

    // Données initiales
    var Array(lx, ly, tx, ty) = readLine.split(" ").map(_.toInt)

    while (true) {
        // Connaissance de l'énergie non nécessaire : On a déjà le chemin le plus court
        // val e = readLine

        // Connaitre le pas d'incrémentation revient à comparer les coordonnées de Thor et de l'éclair
        val c = (lx compare tx, ly compare ty)

        // Il n'y a plus qu'à retrouver la direction à partir de l'incrémentation
        println(directions(c))

        // Màj de la position de Thor
        tx += c._1
        ty += c._2
    }

}
