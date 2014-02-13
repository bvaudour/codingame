import scala.collection.mutable.{HashMap => Map}

object Solution {
    
    val letters = "abcdefghijklmnopqrstuvwxyz"
    val letterPoints = initLetterPoints
    
    def main(args: Array[String]) {
        // Initialization of dictionnary words
        val n = readInt
        val dictionnary = new Array[String](n)
        for (i <- 0 until n) dictionnary(i) = readLine
        
        // Distribution of letters
        val availableLetters = countLetters(readLine)
        
        // For each word of dictionnary, we verify that we can use it and how much points it does
        var maxPoint = 0
        var wordMax = ""
        for (word <- dictionnary) {
           if (isUsable(word, availableLetters)) {
               val c = countPoint(word)
               if (c > maxPoint) {
                   maxPoint = c
                   wordMax = word
               }
           }
        }
        
        println(wordMax)
    }

    // Initialize map of points by letter
    def initLetterPoints: Map[Char, Int] = {
        val out = new Map[Char, Int]
        "eaionrtlsu".foreach(c => out(c) = 1)
        "dg".foreach(c => out(c) = 2)
        "bcmp".foreach(c => out(c) = 3)
        "fhvwy".foreach(c => out(c) = 4)
        "k".foreach(c => out(c) = 5)
        "jx".foreach(c => out(c) = 8)
        "qz".foreach(c => out(c) = 10)
        out
    }
    
    // Count points of a word
    def countPoint(word: String): Int = {
        var out = 0
        word.foreach(c => out += letterPoints(c))
        out
    }
    
    // Count letters of a word grouped by letter type
    def countLetters(word: String): Map[Char, Int] = {
        val out = new Map[Char, Int]
        letters.foreach(c => out(c) = 0)
        word.foreach(c => out(c) += 1)
        out
    }
    
    // Verify if a word can be used
    def isUsable(word: String, distribution: Map[Char, Int]): Boolean = {
        val count = countLetters(word)
        for (c <- letters)
            if (count(c) > distribution(c)) return false
        true
    }
    
}
