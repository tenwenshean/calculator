
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.0"

lazy val root = (project in file("."))
  .settings(
    name := "Calculator",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "2.1.1"
    )

  )

