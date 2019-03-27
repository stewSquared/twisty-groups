package cube

import net.alasc.perms._
import cats.kernel.Group
import cats.syntax.group._

final case class CO private(os: Vector[Int]) {
  override def toString = os.map {
    case 0 => "0"
    case 1 => "+"
    case 2 => "-"
  }.mkString("CO(", ",", ")")

  def permute(perm: Perm): CO =
    CO((1 to 8).toVector.map(i => os(perm.image(i) - 1)))
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

final case class CubeState(cp: Perm, co: CO) {
  import CubeState._
  import CubeState.CubeStateGroup

  def u = CubeStateGroup.combine(this, up)
  def d = this |+| down
  def r = this |+| right
  def l = this |+| left
  def f = this |+| front
  def b = this |+| back

  def u3 = this |-| up
  def d3 = this |-| down
  def r3 = this |-| right
  def l3 = this |-| left
  def f3 = this |-| front
  def b3 = this |-| back

  def u2 = this.u |+| up
  def d2 = this.d |+| down
  def r2 = this.r |+| right
  def l2 = this.l |+| left
  def f2 = this.f |+| front
  def b2 = this.b |+| back
}

object CubeState {
  val id = CubeState(
    cp = Perm.id,
    co = Group.empty[CO]
  )

  implicit object CubeStateGroup extends cats.kernel.Group[CubeState] {

    def combine(x: CubeState, y: CubeState) = CubeState(
      x.cp |+| y.cp,
      x.co.permute(y.cp.inverse) |+| y.co
    )
    def empty = CubeState.id
    def inverse(a: cube.CubeState) = CubeState(
      a.cp.inverse,
      a.co.permute(a.cp).inverse
    )
  }

  val up    = CubeState(cp = Perm(1,2,3,4), co = CO(0,0,0,0,0,0,0,0))
  val down  = CubeState(cp = Perm(5,6,7,8), co = CO(0,0,0,0,0,0,0,0))
  val right = CubeState(cp = Perm(1,4,5,8), co = CO(2,0,0,1,2,0,0,1))
  val left  = CubeState(cp = Perm(2,3,6,7), co = CO(0,1,2,0,0,1,2,0))
  val front = CubeState(cp = Perm(1,8,7,2), co = CO(1,2,0,0,0,0,1,2))
  val back  = CubeState(cp = Perm(3,4,5,6), co = CO(0,0,1,2,1,2,0,0))
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
