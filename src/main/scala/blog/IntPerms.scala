package blog
package object perms2 {

  type Perm[P] = Map[P, P]

  object Perm {
    def apply[P](pairs: (P, P)*): Perm[P] = {
      val domain = pairs.map(_._1).toSet
      val range = pairs.map(_._1).toSet
      require(domain.size == pairs.size, "Domain must be a set.")
      require(range.size == pairs.size, "Range must be a set.")
      require(domain == range, "Domain must match range: $domain != $range")
      new Map.WithDefault(Map(pairs: _*), identity)
    }

    // def apply[P](p: P => P): Perm[P] = {
    //   apply(A -> p(A), B -> p(B), C -> p(C))
    // }
  }
  
  import cats._
  import cats.implicits._

  case class Mod6 private(value: Int)

  object Mod6 {
    def apply(n: Int) = {
      val mod6 = n % 6
      if (mod6 >= 0) new Mod6(mod6)
      else new Mod6(mod6 + 6)
    }

    implicit object Mod6Group extends Group[Mod6] {
      override def empty = Mod6(0)
      override def inverse(n: Mod6) = apply(-n.value)
      override def combine(m: Mod6, n: Mod6) = apply(n.value + m.value)
    }

    val elements = (-12 to 12).map(Mod6(_)).distinct.toList

    def printTable(): Unit = {
      elements.foreach { row =>
        println(s"${row.value}: ${elements.map(col => row |+| col).mkString(" | ")}")
        println("-" * 60)
      }
    }

  }


}
