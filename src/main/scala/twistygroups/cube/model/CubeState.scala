package twistygroups
package cube
package model

import cats.kernel.{Eq, Group}
import cats.syntax.eq._
import cats.syntax.group._
import perms.Perm

final case class CubeState(corners: CornersState, edges: EdgesState) {
  import CubeState._
  import CubeState.CubeStateGroup

  def pretty: String = {
    def emptyIsSolved[A : Group](a: A, message: String): String =
      if (a == Group[A].empty) "solved" else message

    s"""|cp: ${emptyIsSolved(cp, cp.pretty)}
        |co: ${emptyIsSolved(co, co.os.mkString(" "))}
        |ep: ${emptyIsSolved(ep, ep.pretty)}
        |eo: ${emptyIsSolved(eo, eo.os.mkString(" "))}
        |""".stripMargin
  }

  def cp = corners.permutation
  def co = corners.orientation
  def ep = edges.permutation
  def eo = edges.orientation

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
  val id = CubeState(CornersState.id, EdgesState.id)

  implicit object CubeStateEq extends Eq[CubeState] {
    def eqv(x: CubeState, y: CubeState) =
      x.corners === y.corners && x.edges === y.edges
  }

  implicit object CubeStateGroup extends cats.kernel.Group[CubeState] {
    def combine(x: CubeState, y: CubeState) = CubeState(
      x.corners |+| y.corners,
      x.edges |+| y.edges
    )
    def empty = CubeState.id
    def inverse(a: CubeState) = CubeState(a.corners.inverse, a.edges.inverse)
  }

  val up    = CubeState(CornersState(Perm(1,2,3,4), CO(0,0,0,0,0,0,0,0)), EdgesState(Perm( 1, 2, 4, 5), EO(0,0,0,0,0,0,0,0,0,0,0,0)))
  val down  = CubeState(CornersState(Perm(5,6,7,8), CO(0,0,0,0,0,0,0,0)), EdgesState(Perm( 7, 8,10,11), EO(0,0,0,0,0,0,0,0,0,0,0,0)))
  val right = CubeState(CornersState(Perm(1,4,5,8), CO(2,0,0,1,2,0,0,1)), EdgesState(Perm( 5, 6, 8, 9), EO(0,0,0,0,0,0,0,0,0,0,0,0)))
  val left  = CubeState(CornersState(Perm(2,3,6,7), CO(0,1,2,0,0,1,2,0)), EdgesState(Perm(11,12, 2, 3), EO(0,0,0,0,0,0,0,0,0,0,0,0)))
  val front = CubeState(CornersState(Perm(1,8,7,2), CO(1,2,0,0,0,0,1,2)), EdgesState(Perm( 3, 4, 6, 7), EO(0,0,1,1,0,1,1,0,0,0,0,0)))
  val back  = CubeState(CornersState(Perm(3,4,5,6), CO(0,0,1,2,1,2,0,0)), EdgesState(Perm( 9,10,12, 1), EO(1,0,0,0,0,0,0,0,1,1,0,1)))
}
