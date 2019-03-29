package cube

import org.scalatest._
import net.alasc.perms._
import cats.kernel.Group, cats.syntax.group._

class CubeStateSpec extends FlatSpec with Matchers {
  import CubeState.{id, up, right, front, down, left, back}

  "CubeState composition" should "pass some spot checks" in {

    (id |+| right).corners shouldBe CornersState(Perm(1,4,5,8), CO(2,0,0,1,2,0,0,1))

    (id |+| right |+| front).corners shouldBe CornersState(Perm(1,4,5,7,2), CO(1,2,0,1,2,0,2,1))
  }

  "Inverses of basic moves" should "behave sensibly" in {
    import CubeState.{id, up, right, front, down, left, back}
    for (move <- List(up, down, left, right, front, back)) {
      move.inverse shouldBe (move |+| move |+| move)
      Group.combineN(move, 4) shouldBe id
    }
  }

  "Turn helpers" should "transform state consistently with verbose combination" in {
    id.r.r shouldBe Group.combine(right, right)
    id.r.r shouldBe id.r2

    id.r.r.r shouldBe Group.combineN(right, 3)
    id.r.r.r shouldBe id.r3

    id.r.u.r3.u3 shouldBe (right |+| up |-| right |-| up)
  }

  "Half turns" should "not affect corner orientation" in {
    import CubeState.id

    List(id.u2, id.d2, id.l2, id.r2, id.f2, id.b2).map(_.co).toSet shouldBe Set(id.co)
  }

  "RU trigger" should "behave as expected" in {
    val trig = right |+| up |-| right |-| up

    Group.combineN(trig, 6) shouldBe id

    val trig2 = trig |+| trig

    (trig2.d |+| trig2.d |+| trig2.d2) shouldBe CubeState(CornersState(Perm.id, CO(0,0,0,0,0,1,1,1)), EdgesState.id)

    (trig |+| trig |+| trig).edges shouldBe EdgesState.id
  }
}
