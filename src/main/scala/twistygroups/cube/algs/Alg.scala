package twistygroups
package cube
package algs

import cats.syntax.group._
import cats.Eq

import cube.model.CubeState

sealed trait Alg { lhs =>
  def state: CubeState
  def turns: Seq[Turn]
  def moves: Int // move count in half-turn metric

  def prettyTurns = turns.mkString(" ")

/** List of identities used (where [x,y] == x'y'xy and x^y == y'xy):
  * [X]  [x,y]y = y^x
  * [X]  x[x',y] = x'^y
  * [ ]  [y,x] = [x,y]'
  * [X]  [x,y] * [x,z]^y = [x,yz] (meh)
  * [X]  [x,y]^z * [z,y] = [xz,y] (meh)
  * [X]  [x,y]^x' = [y,x'] (useful)
  * [X]  [x,y]^y' = [y',x] (useful)
  * [ ]  Hall-Witt identity (too crazy)
  */
  def simplify: Alg = this match {
    case Comb(x, y) => (x.simplify, y.simplify) match {
      case (a, b) if a == b.inverse => ID
      case (Comm(a, b), b0) if b == b0 => Conj(a, b).simplify
      case (a0, Comm(a, b)) if a == a0.inverse => Conj(b, a0).simplify
      case (Comm(a, b), Conj(b0, Comm(a0, c))) if a == a0 && b == b0 => Comm(a, Comb(b, c)).simplify
      case (Conj(c, Comm(a, b)), Comm(c0, b0)) if b == b0 && c == c0 => Comm(Comb(a, c), b).simplify
      case (a, b) => Comb(a, b)
    }
    case Conj(x, y) => (x.simplify, y.simplify) match {
      case (a, b) if a == b => a
      case (a, b) if a == b.inverse => a.inverse
      case (a0, Comm(a, b)) if a0.inverse == a => Comm(b, a0).simplify
      case (b0, Comm(a, b)) if b0.inverse == b => Comm(b0, a).simplify
      case (a0, Conj(a, b)) => Conj(Comb(a0, a), b).simplify
      case (a, b) => Conj(a, b)
    }
    case Comm(x, y) => (x.simplify, y.simplify) match {
      case (a, b) if a == b => ID
      case (a, b) if a == b.inverse => ID
      case (a, b) => Comm(a, b)
    }
    case turn: Turn => turn
    case ID => ID
  }

  def inverse: Alg = this match {
    case ID => ID
    case Comb(a, b) => Comb(b.inverse, a.inverse)
    case Conj(a, b) => Conj(a, b.inverse)
    case Comm(a, b) => Comm(b.inverse, a.inverse)

    case U => U3
    case D => D3
    case R => R3
    case L => L3
    case F => F3
    case B => B3

    case U2 => U2
    case D2 => D2
    case R2 => R2
    case L2 => L2
    case F2 => F2
    case B2 => B2

    case U3 => U
    case D3 => D
    case R3 => R
    case L3 => L
    case F3 => F
    case B3 => B
  }
}

object Alg {
  // This definition of Eq is debatable
  // Alg is still a group without it
  implicit object CubeAlgEq extends Eq[Alg] {
    def eqv(a: Alg, b: Alg) = a.state == b.state
  }

  def fromString(string: String): Alg = {
    string.split(' ').map[Alg] {
      case "U" => U
      case "D" => D
      case "R" => R
      case "L" => L
      case "F" => F
      case "B" => B

      case "U2" => U2
      case "D2" => D2
      case "R2" => R2
      case "L2" => L2
      case "F2" => F2
      case "B2" => B2

      case "U'" => `U'`
      case "D'" => `D'`
      case "R'" => `R'`
      case "L'" => `L'`
      case "F'" => `F'`
      case "B'" => `B'`
    }.foldLeft[Alg](ID)(Comb.apply)
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
