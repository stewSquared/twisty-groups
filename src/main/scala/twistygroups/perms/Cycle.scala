package perms

case class Cycle(elems: List[Int]) {
  require(elems.length >= 2, "A cycle needs at least two elements")
  // TODO: normalize by min-first
  // TODO: Nicer toString and constructor
  // TODO: extend Perm?

  override def toString = elems.mkString("Cycle(", ", ", ")")

  def toPerm: Perm = {
    Perm(elems(0), elems(1), elems.drop(2): _*)
  }
}
