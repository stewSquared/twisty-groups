## Usage

`sbt run` presents commutator solutions for corner permutations

`sbt console` loads a console with convenient imports

```scala
val p = Perm(1,2,3)(4,5,6) // creating a Perm
p * p
p.inverse

val alg = R.U.R3.U3 // Alternatively, use *
val comm = Comm(alg, D).conjBy(U2) // conjugated commutator

val cornerPerms = comm.state.corners.permutation
```

## Presentations

### SBTB 2021

[Abstract](https://emamo.com/event/scale-by-the-bay/s/solving-the-rubiks-cube-with-group-theory-oGERea)

[Slides org file](presentation-sbtb-2021.org)

[Perm Worksheet](src/main/scala/twistygroups/example/perm.worksheet.sc)

[Solution Worksheet](src/main/scala/twistygroups/example/solution.worksheet.sc)

For additional commentary and examples, see my full [presentation notes](https://raw.github.com/stewSquared/twisty-groups/sbtb-2021/presentation-notes.org)

### NE Scala 2019

[Abstract](https://nescala.io/talks.html#rubiks) 

[Slides org file](presentation.org)

