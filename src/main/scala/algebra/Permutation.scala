package algebra

case class Cycle[T](elems: Vector[T]) {
  override def toString = elems.mkString("Cycle(", " -> ", ")")

  def next(elem: T): T = {
    val i = elems.indexOf(elem)
    if (i < 0) elem else elems((i + 1) % elems.size)
  }
}

case class Permutation(/*private val*/ mapping: Vector[Int]) extends (Int => Int) {
  override def toString = mapping.mkString("Permutation(", " ", ")")

  def apply(n: Int): Int = mapping(n)

  // def unapply? (this.invert.apply())

  // lazy val?
  def inverse = Permutation(0 to mapping.length map mapping.indexOf: _*)

  def cycles: List[Cycle[Int]] = {
    def cycleFrom(start: Int): Cycle[Int] = {
      def findCycle(visited: Vector[Int]): Vector[Int] = {
        val next = apply(visited.last)
        if (next == visited.head) visited
        else findCycle(visited :+ next)
      }
      Cycle(findCycle(Vector(start)))
    }
      
    def findCycles(found: List[Cycle[Int]]): List[Cycle[Int]] = {
      val toVisit = found.map(_.elems).fold(mapping)(_ diff _)
      if (toVisit.isEmpty) found
      else findCycles(cycleFrom(toVisit.min) :: found)
    }

    findCycles(found = Nil).filter(_.elems.size > 1).reverse
  }

  // define instead of extending Function1?
  // def compose(that: Permutation) = (n: Int) => this(that(n))
}

object Permutation {
  def apply(mapping: Int*) = new Permutation(mapping.toVector)
}
