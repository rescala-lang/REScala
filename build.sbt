import Dependencies.*

ThisBuild / version := "0.0.1-SNAPSHOT"
ThisBuild / sbtPlugin := false
ThisBuild / organization := "lore.dsl"

lazy val loreDSL = project
  .in(file("."))
  .settings(
    name := "lore-dsl",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies ++= List(scala3Compiler)
  )

lazy val additionExample = project.in(file("examples/addition"))

lazy val publishPluginLocal = taskKey[Unit]("Publish DSL plugin locally")
publishPluginLocal := {
  Command.process("clean", state.value)
  Command.process("compile", state.value)
  Command.process("package", state.value)
  Command.process("publishLocal", state.value)
}