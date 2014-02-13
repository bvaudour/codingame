// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution
import math._
import scala.util._
import scala.collection.mutable.Buffer

object Solution {

    // Initialisation du dictionnaire d'équivalences
    val dictionnaire = new Array[String](20)
    for (i <- 0 until 20) dictionnaire(i) = ""

    // Initialisation des dimensions d'un chiffre maya
    val Array(l, h) = readLine.split(" ").map(_.toInt)

    def main(args: Array[String]) {
        // Récupération et création du dictionnaire d'équivalences
        for (i <- 0 until h) {
            val s = readLine
            for (j <- 0 until 20) {
                val d = j * l
                val b = new StringBuilder(dictionnaire(j))
                if (b.length != 0) b ++= "\n"
                b ++= s.substring(d, d + l)
                dictionnaire(j) = b.toString()
            }
        }
        
        // Récupération des opérandes
        val o1 = getNombre(readInt / h)
        val o2 = getNombre(readInt / h)
        
        // Calcul du resultat
        var result = readLine match {
            case "+" => o1 + o2
            case "-" => o1 - o2
            case "*" => o1 * o2
            case _   => o1 / o2
        }
        
        // On convertit en base 20
        val result20 = Buffer[Int]()
        while (result != 0L) {
            result20.insert(0, (result % 20).toInt)
            result /= 20
        }

        // On s'assure que le tableau n'est pas vide (s'il est vide, c'est que le résultat est à 0)
        if (result20.isEmpty) result20 += 0
        
        // Sortie
        result20.foreach(i => println(dictionnaire(i)))
    }
    
    // Récupère le prochain chiffre
    def getChiffre: Int = {
        val s = (0 until h).map(i => readLine).mkString("\n")
        for (i <- 0 until 20) if (s == dictionnaire(i)) return i
        return -1 // Ne devrait pas arriver
    }
    
    // Récupère l'opérande constituée de 'i' chiffres Maya :
    //  1. On récupère les i chiffres Maya dans un tableau
    //  2. On convertit ce tableau de chiffres (base 20) en base décimale
    def getNombre(i: Int): Long = {
        var a = new Array[Int](i)
        for (i <- 0 until i) a(i) = getChiffre
        a = a.reverse
        var p = 1L
        var out = 0L
        a.foreach(e => {out += p * e; p *= 20L})
        return out
    }

}
