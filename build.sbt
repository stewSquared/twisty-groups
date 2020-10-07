import Dependencies._

ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "twisty-groups",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0",
    libraryDependencies += "org.typelevel" %% "spire" % "0.17.0-M1",
    libraryDependencies += scalaTest % Test
  )

initialCommands in console := """|import perms._
                                 |import cats.kernel.Group, cats.syntax.group._
                                 |import twistygroups._, cube.algs._, cube.model.CubeState.id
                                 |implicit val random = new scala.util.Random
                                 |""".stripMargin
