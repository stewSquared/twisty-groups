package blog.cube
package model2

import blog.perms3.Perm

sealed trait Color extends Product with Serializable
case object W extends Color
case object Y extends Color
case object R extends Color
case object O extends Color
case object G extends Color
case object B extends Color

case class CubeState(stickers: Array[Color])

class Alg(private val moves: Vector[FaceTurn] ) { lhs =>
  def inverse: Alg = new Alg(moves.reverseMap(_.inverse))

  def combine(rhs: Alg): Alg = {
    new Alg(Alg.reduce(lhs.moves ++ rhs.moves))
    
  }
  def *(rhs: Alg) = lhs.combine(rhs)
}

object Alg {
  def comm(x: Alg, y: Alg) = x * y * x.inverse * y.inverse

  private def reduce(moves: Vector[FaceTurn]): Vector[FaceTurn] = {
    ???
  }
}


sealed trait FaceTurn {
  def inverse: FaceTurn = ???
}
case object Up extends FaceTurn
case object Down extends FaceTurn
case object Right extends FaceTurn
case object Left extends FaceTurn
case object Front extends FaceTurn
case object Back extends FaceTurn

