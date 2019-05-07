package blog
package object perms {

  sealed trait Point extends Product with Serializable
  case object A extends Point
  case object B extends Point
  case object C extends Point

  val allPoints = Set(A, B, C)

  type Perm = Map[Point, Point]

  object Perm {
    def apply(pairs: (Point, Point)*): Perm = {
      require(pairs.map(_._1).toSet == allPoints, s"Domain must be $allPoints.")
      require(pairs.map(_._2).toSet == allPoints, s"Range must be $allPoints.")
      Map(pairs: _*)
    }

    def apply(p: Point => Point): Perm = {
      apply(A -> p(A), B -> p(B), C -> p(C))
    }

    def inverse(perm: Perm): Perm = perm.map(_.swap)

    implicit object PermGroup extends cats.Group[Perm] {
      def empty = Map(A -> A, B -> B, C -> C)

      def inverse(p: Perm) = p.map(_.swap)

      def combine(p: Perm, r: Perm) = Perm(p.compose(r))
    }
  }

  val abc = Perm(
    A -> A,
    B -> B,
    C -> C
  )
  val acb = Perm(
    A -> A,
    B -> C,
    C -> B
  )
  val bac = Perm(
    A -> B,
    B -> A,
    C -> C
  )
  val bca = Perm(
    A -> B,
    B -> C,
    C -> A
  )
  val cab = Perm(
    A -> C,
    B -> A,
    C -> B
  )
  val cba = Perm(
    A -> C,
    B -> B,
    C -> A
  )

  val allPerms = Set(abc, acb, bac, bca, cab, cba)

  def printTriangle(perm: Perm): Unit = {
    println(s"  ${perm(A)}  ")
    println(" / \\ ")
    println(s"${perm(B)}---${perm(C)}")
  }
}
