package cube

import cats.kernel.Group
import cats.syntax.group._
import net.alasc.perms._
 
final case class EO private(os: Vector[Int]) {
  override def toString = os.mkString("EO(", ",", ")")

  def permute(perm: Perm): EO =
    EO((1 to 12).toVector.map(i => os(perm.image(i) - 1)))
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

final case class EdgesState(ep: Perm, eo: EO)

object EdgesState {
  val id = EdgesState(Perm.id, Group.empty[EO])

  implicit object EdgesStateGroup extends cats.kernel.Group[EdgesState] {

    def combine(x: EdgesState, y: EdgesState) = EdgesState(
      x.ep |+| y.ep,
      x.eo.permute(y.ep.inverse) |+| y.eo
    )
    def empty = EdgesState.id
    def inverse(a: cube.EdgesState) = EdgesState(
      a.ep.inverse,
      a.eo.permute(a.ep).inverse
    )
  }
}
