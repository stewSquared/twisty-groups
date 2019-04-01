import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

resolvers += "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven"

lazy val root = (project in file("."))
  .settings(
    name := "twisty-groups",
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0",
    libraryDependencies += "net.alasc" %% "alasc-core" % "0.16.0.3",
    libraryDependencies += scalaTest % Test
  )

scalacOptions += "-Ypartial-unification"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

initialCommands in console := """|import net.alasc.perms._, default._
                                 |import cats.kernel.Group, cats.syntax.group._
                                 |import twistygroups._, cube.algs._, cube.model.CubeState.id
                                 |implicit val random = new scala.util.Random
                                 |""".stripMargin
