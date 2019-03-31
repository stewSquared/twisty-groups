package twistygroups
package cube
package model

import cats.kernel.Group
import cats.syntax.group._
import net.alasc.perms._

final case class CO private(os: Vector[Int]) {
  override def toString = os.map {
    case 0 => "0"
    case 1 => "+"
    case 2 => "-"
  }.mkString("CO(", ",", ")")

  def permute(perm: Perm): CO =
    CO((1 to 8).toVector.map(i => os(perm.invImage(i) - 1)))
}

object CO {
  def apply(os: Vector[Int]): CO = new CO(os.map(Math.floorMod(_, 3)))

  def apply(os: Int*): CO = apply(os.toVector)

  implicit object COGroup extends Group[CO] {
    def empty = CO(Vector.fill(8)(0))
    def inverse(a: CO) = CO(a.os.map(o => -o))
    def combine(a: CO, b: CO) = CO(a.os.zip(b.os).map(Function.tupled(_ + _)))
  }
}

final case class CornersState(permutation: Perm, orientation: CO)

object CornersState {
  val id = CornersState(Perm.id, Group.empty[CO])

  implicit object CornersStateGroup extends cats.kernel.Group[CornersState] {

    def combine(x: CornersState, y: CornersState) = CornersState(
      x.permutation |+| y.permutation,
      x.orientation.permute(y.permutation) |+| y.orientation
    )
    def empty = CornersState.id
    def inverse(a: CornersState) = CornersState(
      a.permutation.inverse,
      a.orientation.permute(a.permutation.inverse).inverse
    )
  }
}

// Alternatives for representing corners:
// type CornersOrientation = List[CornersOrientation] Refined Size[W.`8`.T]
// type CornersOrientation = Map[CornerPiece, CornerOrientation]
// type CornersOrientation = Tuple8(CO, CO, CO, CO, CO, CO, CO, CO)
// case class CornersOrientation(
//   urf: CO, ufl: CO, ulb: CO, ubr: CO,
//   drb: CO, dbl: CO, dlf: CO, dfr: CO
// )

// /**
//   * Use for representing a piece or position unambiguously and
//   * accurately, since indices are arbitrary. Indicies are still useful,
//   * since they're easier to read in permutations.
//   */
// sealed trait CornerPiece
// // URF UFL ULB UBR DRB DBL DLF DFR
// case object URF extends CornerPiece
// case object UFL extends CornerPiece
// case object ULB extends CornerPiece
// case object UBR extends CornerPiece
// case object DRB extends CornerPiece
// case object DBL extends CornerPiece
// case object DLF extends CornerPiece
// case object DFR extends CornerPiece

// object CornerPiece {
//   val numbering = List(URF, UFL, ULB, UBR, DRB, DBL, DLF, DFR)
// }
