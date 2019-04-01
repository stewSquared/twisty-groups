package twistygroups
package cube

import net.alasc.perms.{Perm, Cycle}

package object algs {
  // Aliases for inverse basic turns that are easier to type
  val U3 = `U'`
  val D3 = `D'`
  val R3 = `R'`
  val L3 = `L'`
  val F3 = `F'`
  val B3 = `B'`

  def cornerThreeCycleMap(comms: Seq[Alg]): Map[Cycle, Alg] = {
    comms.groupBy(alg => asThreeCycle(alg.state.cp).get)
      .mapValues(algs => algs.minBy(_.toString.length))
  }

  def threeCycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    comms.groupBy(alg => asThreeCycle(p(alg)).get)
      .mapValues(algs => algs.minBy(_.toString.length))
  }

  def cycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    comms.flatMap(alg => asThreeCycle(p(alg)).map(_ -> alg))
      .groupBy(_._1)
      .mapValues(_.map(_._2).minBy(_.toString.length))
  }

  def edgeCycleMap(comms: Seq[Alg]) = threeCycleMap(comms, _.state.edges.permutation)
}
