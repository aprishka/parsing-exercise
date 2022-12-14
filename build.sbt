import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "parsing",
    libraryDependencies ++= Seq(scalaTest % Test, scalacheck % Test)
  )

libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.10"
