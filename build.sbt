import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "stepik.scala",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "com.lihaoyi" % "ammonite" % "2.0.1" cross CrossVersion.full
    ),
    sourceGenerators in Test += Def.task {
      val file = (sourceManaged in Test) .value / "amm.scala"
      IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
      Seq(file)
    }.taskValue
  )

