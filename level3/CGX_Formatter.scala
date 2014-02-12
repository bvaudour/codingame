package cgx_formatter

// Read inputs from System.in, Write outputs to use print.
// Your class name has to be Solution

object Solution {

  var (tab, quote, beginLine, firstChar) = (0, false, false, true)

  def main(args: Array[String]) {
    // Lecture des données
    val n = readInt()
    val brut = new Array[Array[Char]](n)
    for (i <- 0 until n) brut(i) = readLine.trim.toCharArray
    //Analyse et impression
    for (t <- brut) {
      for (c <- t) {
        if (isPrintable(c)) {
          if (retourChariot(c)) printTab
          print(c)
          if (!quote && c == '(') tab += 4
          if (c == '\'') quote = !quote
          firstChar = false
        }
      }
    }
  }

  def printTab {
    println
    for (e <- 0 until tab) print(' ')
  }

  def retourChariot(c: Char): Boolean = {
    if (quote) return false
    var b = false
    c match {
      case '(' => {
        beginLine = true
        b = if (firstChar) false else true
      }
      case ';' => {
        beginLine = true
        b = false
      }
      case ')' => {
        tab -=4
        beginLine = true
        b = true
      }
      case _ => {
        b = beginLine
        beginLine = false
      }
    }
    b
  }

  def isPrintable(c: Char): Boolean = quote || !c.isWhitespace

}
