package twistygroups
package cube

import perms.{Cycle, Perm, PermOps}

package object algs {
  // Aliases for inverse basic turns that are easier to type
  val U3 = `U'`
  val D3 = `D'`
  val R3 = `R'`
  val L3 = `L'`
  val F3 = `F'`
  val B3 = `B'`

  def threeCycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    // This assumes the perm represented by each alg is a single 3-cycle
    comms.groupBy(alg => p(alg).asThreeCycles.get.head)
      .view.mapValues(algs => algs.minBy(_.toString.length)).toMap
  }

  def cycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    comms.flatMap(alg => p(alg).asThreeCycles.map(_.head -> alg))
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).minBy(_.toString.length)).toMap
  }
}
