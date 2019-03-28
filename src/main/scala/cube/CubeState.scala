package cube

import net.alasc.perms._
import cats.kernel.Group
import cats.syntax.group._

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
