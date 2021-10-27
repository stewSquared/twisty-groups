package perms

case class Cycle(elems: List[Int]) {
  require(elems.length >= 2, "A cycle needs at least two elements")
  // TODO: normalize by min-first
  // TODO: Nicer constructor
  // TODO: extend Perm?

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
