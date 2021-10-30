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

  implicit class AlgSyntax(alg: Alg) {
    def *(rhs: Alg) = Comb(alg, rhs)
    def conjBy(a: Alg) = Conj(a, alg)

    def U = Comb(alg, algs.U)
    def D = Comb(alg, algs.D)
    def R = Comb(alg, algs.R)
    def L = Comb(alg, algs.L)
    def F = Comb(alg, algs.F)
    def B = Comb(alg, algs.B)

    def U2 = Comb(alg, algs.U2)
    def D2 = Comb(alg, algs.D2)
    def R2 = Comb(alg, algs.R2)
    def L2 = Comb(alg, algs.L2)
    def F2 = Comb(alg, algs.F2)
    def B2 = Comb(alg, algs.B2)

    def U3 = Comb(alg, algs.U3)
    def D3 = Comb(alg, algs.D3)
    def R3 = Comb(alg, algs.R3)
    def L3 = Comb(alg, algs.L3)
    def F3 = Comb(alg, algs.F3)
    def B3 = Comb(alg, algs.B3)
  }

  def threeCycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    // This assumes the perm represented by each alg is a single 3-cycle
    comms.groupMapReduce(alg => p(alg).asThreeCycles.get.head)(identity){
      case (a1, a2) => if (a1.moves < a2.moves) a1 else a2
    }
  }

  def cycleMap(comms: Seq[Alg], p: Alg => Perm): Map[Cycle, Alg] = {
    comms.flatMap(alg => p(alg).asThreeCycles.map(_.head -> alg))
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).minBy(_.toString.length)).toMap
  }
}
