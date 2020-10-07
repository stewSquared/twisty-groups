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

  def cornerThreeCycleMap(comms: Seq[Alg]): Map[Cycle, Alg] = {
    // comms are assumed to be 3-cycles made from simple commutators
    comms.groupBy(alg => alg.state.cp.asThreeCycles.get.head)
      .view.mapValues(algs => algs.minBy(_.toString.length)).toMap
  }

  def threeCycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    comms.groupBy(alg => p(alg).asThreeCycles.get.head)
      .view.mapValues(algs => algs.minBy(_.toString.length)).toMap
  }

  def cycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    comms.flatMap(alg => p(alg).asThreeCycles.map(_.head -> alg))
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).minBy(_.toString.length)).toMap
  }

  def edgeCycleMap(comms: Seq[Alg]) = threeCycleMap(comms, _.state.edges.permutation)
}
