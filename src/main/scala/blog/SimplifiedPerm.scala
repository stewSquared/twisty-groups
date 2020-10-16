package blog
package simplePerms

class Perm private(mapping: Map[Int, Int]) extends (Int => Int) {
  require(mapping.values.toSet == mapping.keySet)

  override def apply(k: Int): Int = mapping.getOrElse(k, k)

  private lazy val inverseMapping: Map[Int, Int] = mapping.map(_.swap)

  def image: Set[Int] = mapping.keySet

  def inverse: Perm = new Perm(inverseMapping)

  def invert(k: Int): Int = inverseMapping.getOrElse(k, k)

  def permute[A](vec: Vector[A]): Option[Vector[A]] = {
    if (mapping.keys.max >= vec.length || mapping.keys.min < 0) None
    else Some((0 until vec.length).map(i => vec(this.apply(i))).toVector)
  }

  def compose(that: Perm): Perm = Perm((this.image | that.image).map(k => k -> this(that(k))).toMap)

  def andThen(that: Perm): Perm = that.compose(this)
}

object Perm {
  def apply(mapping: Map[Int, Int]): Perm = {
    new Perm(mapping.filter(Function.tupled(_ != _)))
  }

  def apply(pairs: (Int, Int)*): Perm = new Perm(pairs.toMap)
}
