package twistygroups
package cube
package algs

import cats.syntax.group._
import cats.Eq

import cube.model.CubeState

sealed trait Alg { lhs =>
  def state: CubeState
  def moves: Int // calculate number of moves in half-turn metric

  def *(rhs: Alg) = Comb(lhs, rhs)

  def conjBy(a: Alg) = Conj(a, this)

  def U = Comb(lhs, algs.U)
  def D = Comb(lhs, algs.D)
  def R = Comb(lhs, algs.R)
  def L = Comb(lhs, algs.L)
  def F = Comb(lhs, algs.F)
  def B = Comb(lhs, algs.B)

  def U2 = Comb(lhs, algs.U2)
  def D2 = Comb(lhs, algs.D2)
  def R2 = Comb(lhs, algs.R2)
  def L2 = Comb(lhs, algs.L2)
  def F2 = Comb(lhs, algs.F2)
  def B2 = Comb(lhs, algs.B2)

  def U3 = Comb(lhs, algs.U3)
  def D3 = Comb(lhs, algs.D3)
  def R3 = Comb(lhs, algs.R3)
  def L3 = Comb(lhs, algs.L3)
  def F3 = Comb(lhs, algs.F3)
  def B3 = Comb(lhs, algs.B3)

  def inverse: Alg = this match {
    case ID => ID
    case Comb(a, b) => Comb(b.inverse, a.inverse)
    case Conj(a, b) => Conj(a, b.inverse)
    case Comm(a, b) => Comm(b.inverse, a.inverse)
    case algs.U => algs.U3
    case algs.D => algs.D3
    case algs.R => algs.R3
    case algs.L => algs.L3
    case algs.F => algs.F3
    case algs.B => algs.B3
    case algs.U2 => algs.U2
    case algs.D2 => algs.D2
    case algs.R2 => algs.R2
    case algs.L2 => algs.L2
    case algs.F2 => algs.F2
    case algs.B2 => algs.B2
    case algs.U3 => algs.U
    case algs.D3 => algs.D
    case algs.R3 => algs.R
    case algs.L3 => algs.L
    case algs.F3 => algs.F
    case algs.B3 => algs.B
  }

  def turns: Seq[Turn]
  def prettyTurns = turns.mkString(" ")
}

object Alg {
  implicit object CubeAlgEq extends Eq[Alg] {
    def eqv(a: Alg, b: Alg) = a.state == b.state
  }

  def fromString(string: String): Alg = {
    string.split(' ').map[Alg] {
      case "U" => algs.U
      case "D" => algs.D
      case "R" => algs.R
      case "L" => algs.L
      case "F" => algs.F
      case "B" => algs.B

      case "U2" => algs.U2
      case "D2" => algs.D2
      case "R2" => algs.R2
      case "L2" => algs.L2
      case "F2" => algs.F2
      case "B2" => algs.B2

      case "U'" => algs.`U'`
      case "D'" => algs.`D'`
      case "R'" => algs.`R'`
      case "L'" => algs.`L'`
      case "F'" => algs.`F'`
      case "B'" => algs.`B'`
    }.foldLeft[Alg](algs.ID)(_ * _)
  }
}

case object ID extends Alg {
  val state = CubeState.id
  val moves = 0
  val turns = Vector.empty
}

sealed abstract class Turn(override val state: CubeState) extends Alg {
  val moves = 1
  def turns = Vector(this)
}

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

final case class Comb private[Comb] (a: Alg, b: Alg) extends Alg {
  override def toString = s"$a $b"
  override def state = a.state |+| b.state
  override def moves = a.moves + b.moves
  override def turns = a.turns ++ b.turns
}

object Comb {
  def apply(a: Alg, b: Alg): Alg = (a, b) match {
    case (ID, alg) => alg
    case (alg, ID) => alg
    case (a, b) => new Comb(a, b)
  }
}

final case class Conj private[Conj] (a: Alg, b: Alg) extends Alg {
  override def toString = s"[$a: $b]"
  override def state = a.state |+| b.state |-| a.state
  override def moves = a.moves * 2 + b.moves
  override def turns = a.turns ++ b.turns ++ a.inverse.turns
}

object Conj {
  def apply(a: Alg, b: Alg): Alg = (a, b) match {
    case (ID, alg) => alg
    case (alg, ID) => ID
    case (a, b) => new Conj(a, b)
  }
}

final case class Comm private[Comm] (a: Alg, b: Alg) extends Alg {
  override def toString = s"[$a, $b]"
  override def state = a.state |+| b.state |-| a.state |-| b.state
  override def moves = a.moves * 2 + b.moves * 2
  override def turns = a.turns ++ b.turns ++ a.inverse.turns ++ b.inverse.turns
}

object Comm {
  def apply(a: Alg, b: Alg): Alg = (a, b) match {
    case (ID, alg) => ID
    case (alg, ID) => ID
    case (a, b) => new Comm(a, b)
  }
}
