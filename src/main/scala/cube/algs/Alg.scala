package cube
package algs

import cats.syntax.group._
import cats.Eq

sealed trait Alg { lhs =>
  def state: CubeState

  def *(rhs: Alg) = (lhs, rhs) match {
    case (ID, a) => a
    case (a, ID) => a
    case (a, b) => Comb(a, b)
  }
}

object Alg {
  implicit object CubeAlgEq extends Eq[Alg] {
    def eqv(a: Alg, b: Alg) = a.state == b.state
  }
}

case object ID extends Alg {
  def state = CubeState.id
}

sealed class Turn(override val state: CubeState) extends Alg

case object U extends Turn(CubeState.up)
case object D extends Turn(CubeState.down)
case object R extends Turn(CubeState.right)
case object L extends Turn(CubeState.left)
case object F extends Turn(CubeState.front)
case object B extends Turn(CubeState.back)

case object U2 extends Turn(CubeState.id.u2)
case object D2 extends Turn(CubeState.id.d2)
case object R2 extends Turn(CubeState.id.r2)
case object L2 extends Turn(CubeState.id.l2)
case object F2 extends Turn(CubeState.id.f2)
case object B2 extends Turn(CubeState.id.b2)

case object `U'` extends Turn(CubeState.id.u3)
case object `D'` extends Turn(CubeState.id.d3)
case object `R'` extends Turn(CubeState.id.r3)
case object `L'` extends Turn(CubeState.id.l3)
case object `F'` extends Turn(CubeState.id.f3)
case object `B'` extends Turn(CubeState.id.b3)

// case class Move(override val state: CubeState, name: String) extends Alg {
//   override def toString = name
// }

// object Move {
//   val U = Move(CubeState.up, "U")
// }

// case object U extends Turn { override def state = CubeState.up }
// case object D extends Turn { override def state = CubeState.down }
// case object R extends Turn { override def state = CubeState.right }
// case object L extends Turn { override def state = CubeState.left }
// case object F extends Turn { override def state = CubeState.front }
// case object B extends Turn { override def state = CubeState.back }

// case class Inv(a: Alg) {
//   override def toString = a match {
//     case t: Turn => s"$t'"
//   }
// }

case class Comb(a: Alg, b: Alg) extends Alg {
  override def toString = s"$a $b"
  override def state = a.state |+| b.state
}

case class Conj private (a: Alg, b: Alg) extends Alg {
  override def toString = s"[$a: $b]"
  override def state = a.state |+| b.state |-| a.state
}

case class Comm(a: Alg, b: Alg) extends Alg {
  override def toString = s"[$a, $b]"
  override def state = a.state |+| b.state |-| a.state |-| b.state
}
