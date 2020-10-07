import cats.Eq

package object perms {
  case class Cycle(elems: List[Int]) {
    require(elems.length >= 2, "A cycle needs at least two elements")
    // TODO: normalize by min-first
    // TODO: Nicer toString and constructor
    // TODO: Group instance
    // TODO: extend Perm?

    override def toString = elems.mkString("Cycle(", ", ", ")")

    def toPerm: Perm = {
      Perm(elems(0), elems(1), elems.drop(2): _*)
    }
  }

  implicit object PermEq extends Eq[Perm] {
    def eqv(x: Perm, y: Perm) = x.show == y.show
  }

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

    def isEven: Boolean =
      ((perm.cycles.length % 2) ^ (perm.image.size % 2)) == 0

    def show: String = {
      s"""Perm${cycles.map(_.elems.mkString("(", ",", ")")).mkString}"""
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
