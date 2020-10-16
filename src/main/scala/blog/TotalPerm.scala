package blog
package perms3

import cats._
import cats.implicits._
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

class Perm private(private val mapping: Map[Int, Int]) extends (Int => Int) {
  require(mapping.values.toSet == mapping.keySet, "Image and preimage must be the same.")

  override def apply(k: Int): Int = mapping.getOrElse(k, k)

  override def equals(that: Any): Boolean = that match {
    case p: Perm => p.mapping == mapping
    case _ => false
  }

  override def toString: String = {
    mapping.toSeq.sorted
      .map { case (k, v) => s"$k -> $v"}
      .mkString("Perm(", ", ", ")")
  }

  private lazy val inverseMapping: Map[Int, Int] = mapping.map(_.swap)

  def image: Set[Int] = mapping.keySet

  def inverse: Perm = new Perm(inverseMapping)

  def invert(k: Int): Int = inverseMapping.getOrElse(k, k)

  def permute[A](vec: IndexedSeq[A]): Option[IndexedSeq[A]] = {
    if (image.max >= vec.length || image.min < 0) None
    else Some((0 until vec.length).map(i => vec(this.apply(i))))
  }

  def permuteSeq[A, SA](seq: SeqLike[A, SA])(implicit cbf: CanBuildFrom[SA, A, SA]): Option[SA] = {
    if (this.image.max >= seq.length || this.image.min < 0) None
    else {
      val b: Builder[A, SA] = cbf()
      (0 until seq.length).foreach(i => b += seq(this.apply(i)))
      Some(b.result())
    }
  }

  def compose(that: Perm): Perm = Perm((this.image | that.image).map(k => k -> this(that(k))).toMap)

  def andThen(that: Perm): Perm = that.compose(this)
}

object Perm {
  def apply(mapping: Map[Int, Int]): Perm = {
    new Perm(mapping.filter(Function.tupled(_ != _)))
  }

  def apply(pairs: (Int, Int)*): Perm = apply(Map(pairs: _*))

  def cycle(points: Int*): Perm = {
    if (points.isEmpty) apply(Map.empty[Int, Int])
    else apply(points.zip(points.tail :+ points.head).toMap)
  }

  implicit val PermEq = new Eq[Perm] {
    def eqv(p: Perm, r: Perm): Boolean = p.mapping == r.mapping
  }

  implicit object PermGroup extends Group[Perm] {
    def empty: Perm = Perm(Map.empty[Int, Int])
    def inverse(p: Perm): Perm = p.inverse
    def combine(p: Perm, r: Perm): Perm = p.andThen(r)
  }
}
