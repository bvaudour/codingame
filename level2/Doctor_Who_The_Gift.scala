object Solution {
    def main(args: Array[String]) {

        // Récupération des données
        val (n, c) = (readInt, readInt)
        var budget = new Array[Int](n)
        var somme = 0
        for (i <- 0 until n) {
            budget(i) = readInt
            somme += budget(i)
        }

        // Cas évident…
        if (somme < c) {
            println("IMPOSSIBLE")
            return
        }
 
        // Cas partage possible…
        budget = budget.sorted
        var rest = c // budget restant à allouer
        for (i <- 0 until n) {
           val mean = rest / (n - i) // Moyenne des budgets restants
           val b = math.min(budget(i), mean)
           println(b)
           rest -= b
        }
    }
}
