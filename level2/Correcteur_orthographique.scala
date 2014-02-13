import scala.collection.mutable.{HashMap => Map, Stack}

object Solution {
    val pronoms = initPronoms
    val articles = initArticles
    val verbe = readLine.split(" ")
    
    def main(args: Array[String]) {
        
        // Récupération de la liste des adjectifs
        val nbA = readInt
        val adjectifs = new Array[Array[String]](nbA)
        for (i <- 0 until nbA) adjectifs(i) = readLine.split(" ")
        
        // Récupération de la liste des noms
        val nbN = readInt
        val noms = new Array[Array[String]](nbN)
        for (i <- 0 until nbN) noms(i) = readLine.split(" ")
        
        // Phrase à corriger
        val phrase = readLine.split(" ")
        
        // Correction
        var (tmps, idxA, idxN) = (-1, -1, -1)
        var (genre, mult) = (-1, -1)
        val must_correct = new Stack[(Int, Int)]
        for (i <- 0 until phrase.size) {
            val word = phrase(i)
            if (isPronom(word)) {
                tmps = pronoms(word)
            } else if (isArticle(word)) {
                val p = articles(word)
                genre = p._1
                mult  = p._2
                tmps  = 2 + mult * 3
                idxN = 1 + mult
                if (genre >= 0) idxA = mult + genre * 2
            } else if (isVerbe(word)) {
                phrase(i) = verbe(tmps)
                genre = -1
                mult  = -1
                idxA  = -1
                idxN  = -1
            } else {
                var idx = idxOfWord(word, adjectifs)
                if (idx < 0) {
                    idx = idxOfWord(word, noms)
                    phrase(i) = noms(idx)(idxN)
                    if (genre < 0) {
                        genre = if (noms(idx)(0) == "masculin") 0 else 1
                        idxA = mult + genre * 2
                        while (!must_correct.isEmpty) {
                            val (i1, i2) = must_correct.pop
                            phrase(i1) = adjectifs(i2)(idxA)
                        }
                    }
                } else {
                    if (idxA < 0) {
                        must_correct.push((i, idx))
                    } else {
                        phrase(i) = adjectifs(idx)(idxA)
                    }
                }
            }
        }
        
        // Sortie
        println(phrase.mkString(" "))
    }
    
    def initPronoms: Map[String, Int] = {
        val out = new Map[String, Int]
        out("je") = 0
        out("tu") = 1
        out("il") = 2
        out("elle") = 2
        out("nous") = 3
        out("vous") = 4
        out("ils") = 5
        out("elles") = 5
        out
    }
    
    def initArticles: Map[String, (Int, Int)] = {
        val out = new Map[String, (Int, Int)]
        out("un") = (0, 0)
        out("une") = (1, 0)
        out("le") = (0, 0)
        out("la") = (1, 0)
        out("les") = (-1, 1)
        out("des") = (-1, 1)
        out
    }
    
    def isPronom(word: String) : Boolean = pronoms.keySet.contains(word)
    def isArticle(word: String): Boolean = articles.keySet.contains(word)
    def isVerbe(word: String)  : Boolean = verbe.contains(word)
    
    def idxOfWord(word: String, words: Array[Array[String]]): Int = {
        for (i <- 0 until words.size)
            if (words(i).contains(word)) return i
        -1
    }

}
