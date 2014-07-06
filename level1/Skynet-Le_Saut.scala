object Player extends App {
  def pspeed: Unit = println("SPEED")
  def pslow : Unit = println("SLOW")
  def pjump : Unit = println("JUMP")
  def pwait : Unit = println("WAIT")

  val (r, g, l) = (readInt, readInt, readInt)

  while (true) {
    val (s, x) = (readInt, readInt)
    if (x == r - 1) {
      pjump
    } else if (x < r) {
      if (s <= g)         pspeed
      else if (s > g + 1) pslow
      else                pwait
    } else {
      pslow
    }
  }
}
