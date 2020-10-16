package blog.cube
package model

import blog.perms3.Perm

import cats._
import cats.implicits._

final case class CO private(vector: Vector[Int]) extends AnyVal {
  override def toString = vector.mkString("CO(", " ", ")")
}

object CO {
  def apply(os: Int*): CO = {
    require(os.length == 8, "Orientation must be given for exactly 8 corners.")
    new CO(os.toVector.map(Math.floorMod(_, 3)))
  }
  val solved = new CO(Vector.fill(8)(0))
}

final case class EO private(vector: Vector[Int]) extends AnyVal {
  override def toString = vector.mkString("EO(", " ", ")")
}

object EO {
  def apply(os: Int*): EO = {
    require(os.length == 12, "Orientation must be given for exactly 12 edges.")
    new EO(os.toVector.map(Math.floorMod(_, 2)))
  }
  val solved = new EO(Vector.fill(12)(0))
}


final case class CornersState(permutation: Perm, orientation: CO) {
  require(permutation.image.subsetOf((0 until 8).toSet),
    "Permutation must only permute 8 corners.")
}

object CornersState {
  val solved = CornersState(Perm(), CO.solved)
}

final case class EdgesState(permutation: Perm, orientation: EO) {
  require(permutation.image.subsetOf((0 until 8).toSet),
    "Permutation must only permute 12 edges.")
}

object EdgesState {
  val solved = EdgesState(Perm(), EO.solved)
}

final case class CubeState(corners: CornersState, edges: EdgesState)

object CubeState {
  val solved = CubeState(CornersState.solved, EdgesState.solved)
}

sealed trait Alg {
  def andThen(alg: Alg) = Comb(this, alg)
  def >>(alg: Alg) = this andThen alg

  def u = this andThen Face.U(1)
  def u2 = this andThen Face.U(2)
  def u3 = this andThen Face.U(3)
}

case object ID extends Alg

case class Comb(lhs: Alg, rhs: Alg) extends Alg
object Comb {
  def apply(lhs: Alg, rhs: Alg): Alg = (lhs, rhs) match {
    case (ID, alg) => alg
    case (alg, ID) => alg
    case (FaceTurn(f1, t1), FaceTurn(f2, t2)) if f1 == f2 => FaceTurn(f1, t1 + t2)
    case (a, b) if a == b => ID
    case (a, b) => new Comb(a, b)
  }
}

case class FaceTurn private(face: Face, turns: Int) extends Alg {
  override def toString = if (turns == 3) s"$face'" else s"$face$turns"
}

sealed trait Face { face =>
  def apply(turns: Int): Alg = {
    val turnsMod4 = Math.floorMod(turns, 4)
    if (turnsMod4 == 0) ID
    else FaceTurn(face, turnsMod4)
  }
}

object Face {
  case object U extends Face
  case object D extends Face
  case object R extends Face
  case object L extends Face
  case object F extends Face
  case object B extends Face
}





// case class Right(turns: Int) extends FaceTurn
// object Right {
//   def apply(turns: Int): Right = new Right(Math.floorMod(turns, 4))
// }

// case class Up(turns: Int) extends FaceTurn
