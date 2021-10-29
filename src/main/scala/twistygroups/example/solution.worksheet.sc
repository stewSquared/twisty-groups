import twistygroups.cube.algs._
import perms._

////////////////////////////////////////////////////////////////
// An example scramble
////////////////////////////////////////////////////////////////

val scramble = R.D3.F2.U3.R2.U.B2.D3.F2.D.L2.F2.L2.B.U.F2.U3.B.D.R

println(scramble.state)

println(scramble.state.cp.pretty)

////////////////////////////////////////////////////////////////
// Goal: Generate commutators for all these triples
////////////////////////////////////////////////////////////////

val allCycles = for {
  i <- 3 to 8
  j <- 2 until i
  k <- 1 until j
  p <- Array(Perm(k,j,i), Perm(k,i,j))
} yield p

allCycles.size

val simpleComm = Comm(Conj(R2 * U, R2), D2).conjBy(U)

simpleComm.turns.mkString(" ")

(Conj(ID, simpleComm)).state.cp

val moreComms = for {
  r2 <- List(R2, L2)
  u <- List(U, U3)
  d <- List(D, D2, D3)
  topBotFlip <- List(ID, B2, L2, F2, R2)
  topBotAlign <- List(ID, U, U2, U3, D, D2, D3)
} yield Comm(Conj(r2 * u, r2), d).conjBy(topBotFlip * topBotAlign)

assert(moreComms.forall(_.state.cp.image.sizeIs == 3))

moreComms.size
moreComms.distinctBy(_.state.cp).size
moreComms.map(_.state.cp.pretty) foreach println

allCycles.diff(moreComms.map(_.state.cp)).foreach(println)

val solutionCycles = scramble.state.cp.inverse.asThreeCycles.get.reverse

println(scramble.state.cp.pretty)

println(solutionCycles.map(_.pretty).mkString)

val steps: List[Alg] = solutionCycles.map{ cycle =>
  moreComms.filter(cycle == _.state.cp.cycles.head).minBy(_.moves)
}

steps.foreach(println)

println(scramble.prettyTurns)

println(steps(0).prettyTurns)
println(steps(1).prettyTurns)
println(steps(2).prettyTurns)
