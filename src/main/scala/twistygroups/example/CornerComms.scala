package twistygroups
package example

import cats.syntax.eq._
import cats.syntax.group._

import cube.algs._
import perms._

trait CornerComms {

  ////////////////////////////////////////////////////////////////
  // Goal: Generate commutators for these triples
  ////////////////////////////////////////////////////////////////

  val allCycles = for {
    i <- 3 to 8
    j <- 2 until i
    k <- 1 until j
    p <- Array(Perm(k,j,i), Perm(k,i,j))
  } yield p

  ////////////////////////////////////////////////////////////////
  // Part 1: expand on [[R2 U: R2], D]
  // alg"(R2 U R2 U` R2) D (R2 U R2 U` R2) D'"
  ////////////////////////////////////////////////////////////////

  // this is our model commutator,
  // of which we generate variations
  val simpleComm = Comm(Conj(R2 * U, R2), D)

  val allComms = {
    val twoOnTop = for {
      u <- List(U, U3) // z-axis mirror
      r2 <- List(L2, R2) // x-axis mirror
      d <- List(D, D2, D3)
      dConj <- List(ID, D, D2, D3)
    } yield Conj(dConj, Comm(Conj(r2 * u, r2), d))

    val oneOnTop = for { // y-axis mirror of oneOnTop
      d <- List(D, D3) // z-axis mirror
      r2 <- List(L2, R2) // x-axis mirror
      uConj <- List(ID, U, U2, U3)
      u <- List(U, U2, U3)
    } yield Conj(uConj, Comm(Conj(r2 * d, r2), u))

    (twoOnTop ++ oneOnTop).flatMap(c => List(ID, R2).map(c.conjBy))
  }

  val commIndex: Map[Cycle, Alg] = {
    allComms.groupMapReduce(_.state.cp.asThreeCycles.get.head)(_.simplify){
      case (a1, a2) => if (a1.moves < a2.moves) a1 else a2
    }
  }

  def solutionCycles(scramble: Alg): List[Cycle] = {
    val p = scramble.state.corners.permutation
    assert(p.isEven, "You can only solve even permutations with commutators")
    // Note: We reverse the cycles at the end
    // because when we apply them physically,
    // we use `andThen` rather than `compose`
    p.inverse.asThreeCycles.get.reverse
  }

  def solution(scramble: Alg): List[Alg] = {
    solutionCycles(scramble).map(commIndex)
  }

  ////////////////////////////////////////////////////////////////
  // Presentation Output Helpers
  ////////////////////////////////////////////////////////////////

  def displayComm(comm: Alg): Unit = {
    println(s"${comm.state.cp.pretty}: $comm")
  }

  def showProblem(scramble: Alg): Unit = {
    println("scramble: " + scramble)
    // println(scramble.state.edges)
    println(s"permutation: ${scramble.state.cp.pretty}")
    println(s"orientation: ${scramble.state.co.toString.drop(2)}")
  }
}

object CornerComms extends CornerComms

object Main extends App with CornerComms {
  ////////////////////////////////////////////////////////////////
  // Putting it all together!
  ////////////////////////////////////////////////////////////////

  val scrambles = List( // Corner-only scrambles
    R.D3.F2.U3.R2.U.B2.D3.F2.D.L2.F2.L2.B.U.F2.U3.B.D.R,
    D2.U.R2.D2.U3.B2.R2.B2.R.D3.B2.D.B2.R.B2,
    B3.D2.B.R2.F2.R2.D2.U2.B3.D2.U2.L2.R.F3.L.F3.L.F3.R3,
    R.B2.F2.U2.L2.F2.R2.U.R2.U3.B2.L2.R.D.L2.R3.D2.R.D,
    F2.U3.L2.F2.D3.R2.D2.B2.U.F2.U.B2.R3.F2.U3.B2.R2.D.U2.L
  )

  scrambles.foreach { scramble =>
    val steps = solution(scramble)
    assert((scramble.state.cp >> steps.map(_.state.cp).reduce(_ >> _)) === Perm())
    scala.io.StdIn.readLine("Show next [ENTER]:")
    println()
    println("================================================================")
    println()
    showProblem(scramble)
    println()
    steps.foreach(displayComm)
    println()
    println("solution:")
    steps.foreach(s => println(s.prettyTurns))
    println()
    println("================================================================")
    println()
  }
}
