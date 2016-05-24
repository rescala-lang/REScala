name := "examples-reswing"

organization := "de.tuda.stg"

version := "0.0.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.11+",
  "de.tuda.stg" %% "rescala" % "0.17.0-SNAPSHOT",
  "de.tuda.stg" %% "reswing" % "0.17.0-SNAPSHOT"
)
