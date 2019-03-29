package cube

import cats.kernel.Group
import cats.syntax.group._
import net.alasc.perms._
 
final case class EO private(os: Vector[Int]) {
  override def toString = os.mkString("EO(", ",", ")")

  def permute(perm: Perm): EO =
    EO((1 to 12).toVector.map(i => os(perm.invImage(i) - 1)))
}

object EO {
  def apply(os: Vector[Int]): EO = new EO(os.map(_ & 1))

  def apply(os: Int*): EO = apply(os.toVector)

  implicit object EOGroup extends Group[EO] {
    def empty = EO(Vector.fill(12)(0))
    def inverse(a: EO) = a
    def combine(a: EO, b: EO) = EO(a.os.zip(b.os).map(Function.tupled(_ ^ _)))
  }
}

final case class EdgesState(permutation: Perm, orientation: EO)

object EdgesState {
  val id = EdgesState(Perm.id, Group.empty[EO])

  implicit object EdgesStateGroup extends cats.kernel.Group[EdgesState] {

    def combine(x: EdgesState, y: EdgesState) = EdgesState(
      x.permutation |+| y.permutation,
      x.orientation.permute(y.permutation) |+| y.orientation
    )
    def empty = EdgesState.id
    def inverse(a: EdgesState) = EdgesState(
      a.permutation.inverse,
      a.orientation.permute(a.permutation.inverse).inverse
    )
  }
}
