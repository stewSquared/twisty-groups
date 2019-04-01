import net.alasc.perms._
import cats.kernel.Group
import cats.syntax.group._

package object twistygroups {
  def isEven(perm: Perm) =
    ((perm.toCycles.seq.length % 2) ^ (perm.nMovedPoints % 2)) == 0

  def asTranspositions(perm: Perm): List[Perm] = asTranspositionsTailRec(perm, Nil)

  private def asTranspositionsTailRec(perm: Perm, transpositions: List[Perm]): List[Perm] = {
    perm.smallestMovedPoint.fold(transpositions) { smallest =>
      val t = Perm.transposition(perm.invImage(smallest), smallest)
      asTranspositionsTailRec(perm |-| t, t :: transpositions)
    }
  }

  def asThreeCycles(perm: Perm): Option[List[Cycle]] = {
    if (!isEven(perm)) None else Some {
      val swaps = if (perm.toCycles.seq.length <= 1) {
        asTranspositions(perm)
      } else {
        val cycleSmallestPoints = perm.toCycles.seq.map(_.seq.min)
        val crossPerm = Perm(perm.toCycles.seq.map(_.seq.min): _*)
        asTranspositions(perm |+| crossPerm) ++ asTranspositions(crossPerm.inverse)
      }

      swaps.grouped(2).map(Group.combineAll(_)).toList.flatMap(asThreeCycle)
    }
  }

  def asThreeCycle(perm: Perm): Option[Cycle] = {
    val cycles = perm.toCycles.seq
    if (cycles.length > 1) None
    else cycles.headOption.filter(_.length == 3)
  }

  def asCycle(perm: Perm): Option[Cycle] = {
    val cycles = perm.toCycles.seq
    if (cycles.length > 1) None
    else cycles.headOption
  }
}
