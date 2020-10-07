package twistygroups
package cube
package model

import cats.kernel.{Eq, Group}
import cats.syntax.eq._
import cats.syntax.group._

import perms.Perm
import perms.PermEq

final case class EO private(os: Vector[Int]) {
  override def toString = os.mkString("EO(", ",", ")")

  def permute(perm: Perm): EO =
    // TODO verify perm no larger than 12
    // TODO consider 0 index
    EO(perm.permute(0 +: os).get.tail)
    // EO((1 to 12).toVector.map(i => os(perm.invert(i) - 1)))
}

object EO {
  def apply(os: Vector[Int]): EO = new EO(os.map(_ & 1))

  def apply(os: Int*): EO = apply(os.toVector)

  implicit val EOEq: Eq[EO] = Eq.fromUniversalEquals

  implicit object EOGroup extends Group[EO] {
    def empty = EO(Vector.fill(12)(0))
    def inverse(a: EO) = a
    def combine(a: EO, b: EO) = EO(a.os.zip(b.os).map(Function.tupled(_ ^ _)))
  }
}

final case class EdgesState(permutation: Perm, orientation: EO)

object EdgesState {
  val id = EdgesState(Perm(), Group.empty[EO])

  implicit object EdgesStateEq extends Eq[EdgesState] {
    def eqv(x: EdgesState, y: EdgesState) =
      x.permutation === y.permutation && x.orientation === y.orientation
  }

  implicit object EdgesStateGroup extends cats.kernel.Group[EdgesState] {

    def combine(x: EdgesState, y: EdgesState) = EdgesState(
      y.permutation |+| x.permutation,
      y.orientation |+| x.orientation.permute(y.permutation)
    )
    def empty = EdgesState.id
    def inverse(a: EdgesState) = EdgesState(
      a.permutation.inverse,
      a.orientation.permute(a.permutation.inverse).inverse
    )
  }
}
