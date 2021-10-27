package perms

case class Cycle private (elems: List[Int]) {
  // TODO: extend Perm?
  // TODO: Helpful error for attempted 1-cycle

  override def toString = elems.mkString("Cycle(", ", ", ")")

  // Show in a normalized cyclic (one-line) notation
  lazy val pretty: String = {
    val (left, right) = elems.splitAt(elems.indexOf(elems.min))
    right.concat(left).mkString("(", " ", ")")
  }

  def toPerm: Perm = {
    Perm(elems(0), elems(1), elems.drop(2): _*)
  }
}

object Cycle {
  // normalize by min-first
  def apply(elems: List[Int]): Cycle = {
    require(elems.length >= 2, "A cycle needs at least two elements")
    val (left, right) = elems.splitAt(elems.indexOf(elems.min))
    new Cycle(right ++ left)

  }

  def apply(n0: Int, n1: Int, ns: Int*): Cycle = {
    val cycle = n0 +: n1 +: ns
    require(
      cycle.size == cycle.distinct.size,
      "Cycle must not repeat elements"
    )
    apply(n0 :: n1 :: ns.toList)
  }
}
