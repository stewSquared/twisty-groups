package  blog
package object perms3 {

  sealed trait Point extends Product with Serializable
  case object A extends Point
  case object B extends Point
  case object C extends Point

  sealed trait Perm3 extends Function1[Point, Point] {
    def inverse: Perm3
    // def compose
  }

  object ABC extends Perm3 {
    def inverse = ABC

    def apply(p: Point): Point = p match {
      case A => A
      case B => B
      case C => C
    }
  }

  object BCA extends Perm3 {
    def inverse = CAB

    def apply(p: Point): Point = p match {
      case A => B
      case B => C
      case C => A
    }
  }

  object CAB extends Perm3 {
    def inverse = BCA

    def apply(p: Point): Point = p match {
      case A => C
      case B => A
      case C =>  B
    }
  }
}
