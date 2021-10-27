import cats.Eq

package object perms {
  implicit val PermEq = Eq.fromUniversalEquals

  implicit class PermOps(perm: Perm) {
    // TODO: make this safe (cycle requires at least two elements)
    def cycleFrom(point: Int): Cycle = cycleFrom(point, visited = Nil)

    private def cycleFrom(point: Int, visited: List[Int]): Cycle = {
      val next = perm(point)
      if (visited.contains(next)) Cycle((point :: visited).reverse)
      else cycleFrom(next, point :: visited)
    }

    // use lazy val
    def cycles: List[Cycle] = findCycles(toVisit = perm.image, cycles = Nil).reverse

    private def findCycles(toVisit: Set[Int], cycles: List[Cycle]): List[Cycle] = {
      toVisit.minOption.fold(cycles) { min =>
        val cycle = cycleFrom(min)
        findCycles(toVisit -- cycle.elems, cycle :: cycles)
      }
    }

    def inv: Perm = perm.inverse

    def *(that: Perm): Perm = perm.compose(that)

    def >>(that: Perm): Perm = perm.andThen(that)

    def isEven: Boolean =
      ((perm.cycles.length % 2) ^ (perm.image.size % 2)) == 0

    // Show the perm in cyclic (one-line) notation
    // lazy val pretty: String = cycles.map(_.pretty).mkString
    lazy val pretty: String = {
      cycles
        .map(_.elems.mkString(" "))
        .mkString("(", ")(", ")")
    }

    def asTranspositions: List[Perm] = PermOps.asTranspositions(perm)

    def asThreeCycles: Option[List[Cycle]] = {
      PermOps.asThreeCycles(perm).map { cycles =>
        cycles.flatMap { perm =>
          assert(perm.image.size == 3, s"was not a 3-cycle: $perm")
          perm.cycles.headOption
        }
      }
    }
  }

  object PermOps {
    import cats.syntax.group._
    import cats.kernel.Group

    def asTranspositions(perm: Perm): List[Perm] = { // : List[Cycle]
      def factor(perm: Perm, factors: List[Perm]): List[Perm] = {
        perm.image.minOption.fold(factors){ min =>
          val t = Perm(min, perm(min))
          factor(perm |-| t, t :: factors)
        }
      }
      factor(perm, Nil)
    }

    def asThreeCycles(perm: Perm): Option[List[Perm]] = { // : Option[List[Cycle]]
      if (!perm.isEven) None else Some {
        val swaps = if (perm.cycles.length < 2) {
          asTranspositions(perm)
        } else {
          val mergeCycle = Cycle(perm.cycles.map(_.elems.min)).toPerm
          val mainCycle = mergeCycle |+| perm
          asTranspositions(mergeCycle.inverse) ++ asTranspositions(mainCycle)
        }
        swaps.grouped(2).map(Group.combineAll(_)).toList
      }
    }
  }
}
