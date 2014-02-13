import math._
import scala.collection.mutable.Stack

object Solution {

    // Used to know idx of square, list of accessible scares and distance from start
    class Node(val idx: Int, val neighbors: Seq[Int], var dist: Int = -1)

    def main(args: Array[String]) {

    // Initialize data
        val n = readInt
        val square = new Array[Node](n)

        // Index of start and end squares
        var (start, end) = (-1, -1)
        for (i <- 0 until n) {
            val l = readLine
            square(i) = l match {
                case "S" => {
                    start = i
                    val s: Seq[Int] = (i + 1) to math.min(n - 1, i + 6)
                    new Node(i, s, 0)
                }
                case "E" => {
                    end = i
                    new Node(i, Seq[Int]())
                }
                case "R" => {
                    val s: Seq[Int] = (i + 1) to math.min(n - 1, i + 6)
                    new Node(i, s)
                }
                case _ => {
                    val next = i + l.toInt
                    val s = if (next >= 0 && next < n) Seq(next) else Seq[Int]()
                    new Node(i, s)
                }
            }
        }

        // Compute max movements with Dijkstra algorithm
        val p = new Stack[Node]
        p.push(square(start))
        while (!p.isEmpty) {
            val node = p.pop
            val d = node.dist + 1
            for (i <- node.neighbors) {
                val neigh = square(i)
                if (neigh.dist < 0 || neigh.dist > d) {
                    neigh.dist = d
                    p.push(neigh)
                }
            }
        }
        val d = square(end).dist
        println(if (d < 0) "impossible" else d)
    }
}
