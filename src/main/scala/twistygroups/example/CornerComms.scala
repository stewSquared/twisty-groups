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

  val allTriples = for {
    i <- 3 to 8
    j <- 2 until i
    k <- 1 until j
  } yield (k,j,i)

  ////////////////////////////////////////////////////////////////
  // Part 1: expand on [[R2 U: R2], D]
  // alg"(R2 U R2 U` R2) D (R2 U R2 U` R2) D'"
  ////////////////////////////////////////////////////////////////

  val simpleComm = Comm(Conj(R2 * U, R2), D)

  val cheat = {
    val twoOnTop = for {
      u <- List(U, U3) // z-axis mirror
      r2 <- List(L2, R2) // x-axis mirror
      dConj <- List(ID, D, D2, D3)
      dGap <- List(D, D2, D3)
    } yield Conj(dConj, Comm(Conj(r2 * u, r2), dGap))
    
    val oneOnTop = for { // y-axis mirror of oneOnTop
      d <- List(D, D3) // z-axis mirror
      r2 <- List(L2, R2) // x-axis mirror
      uConj <- List(ID, U, U2, U3)
      uGap <- List(U, U2, U3)
    } yield Conj(uConj, Comm(Conj(r2 * d, r2), uGap))

    twoOnTop ++ oneOnTop
  }

  val allComms: List[Alg] = cheat

  ////////////////////////////////////////////////////////////////
  // Part 2: Putting it all together!
  ////////////////////////////////////////////////////////////////

  val scrambles = List(
    // Corner-only scrambles
    R.D3.F2.U3.R2.U.B2.D3.F2.D.L2.F2.L2.B.U.F2.U3.B.D.R,
    D2.U.R2.D2.U3.B2.R2.B2.R.D3.B2.D.B2.R.B2,
    B3.D2.B.R2.F2.R2.D2.U2.B3.D2.U2.L2.R.F3.L.F3.L.F3.R3,
    R.B2.F2.U2.L2.F2.R2.U.R2.U3.B2.L2.R.D.L2.R3.D2.R.D,
    F2.U3.L2.F2.D3.R2.D2.B2.U.F2.U.B2.R3.F2.U3.B2.R2.D.U2.L
  )

  val commIndex: Map[Cycle, Alg] =
    threeCycleMap(allComms, _.state.corners.permutation)
}

object CornerComms extends CornerComms

object Main extends App with CornerComms {

  def display(comms: Seq[Alg]): Unit = {
    comms.map(c => s"${c.state.cp}: $c") foreach println
  }

  // display(allComms)

  def printCornerPermSolution(scramble: Alg): Unit = {
    println("================================")
    println()
    println("scramble: " + scramble)
    // println(scramble.state.edges)
    println(s"permutation: ${scramble.state.cp.pretty}")
    println(s"orientation: ${scramble.state.co.toString.drop(2)}")
    println()

    // We reverse the list of cycles, since we combine them with
    // andThen rather than compose
    val solutionCycles: List[Cycle] =
      scramble.state.corners.permutation.inverse.asThreeCycles.get.reverse

    assert((scramble.state.cp >> solutionCycles.map(_.toPerm).reduce(_ >> _)) === Perm())

    val solutionAlgs: List[(Cycle, Alg)] = solutionCycles.map { cycle => cycle ->
      commIndex.get(cycle).getOrElse {
        val conjCycleByR2 = {
          (R2.state.cp |+| cycle.toPerm |+| R2.state.cp).cycles.head
        }
        Conj(R2, commIndex(conjCycleByR2))
      }
    }

    solutionAlgs.foreach { case (cycle, alg) =>
      println(s"${cycle.pretty}: $alg")
    }
    println()

    println("solution:")
    solutionAlgs.foreach { case (_, alg) =>
      println("  " + alg.turns.mkString(" "))
    }
    println()
    io.StdIn.readLine()
  }

  scrambles foreach printCornerPermSolution
}
