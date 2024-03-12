lazy val root = project
  .in(file("."))
  .settings(
    name := "lore-dsl",
    organization := "de.tu-darmstadt.stud",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.3.1",
    sbtPlugin := false,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % "3.3.1" % "provided",
    libraryDependencies += "de.tu-darmstadt.stg" %% "lore" % "0.2-SNAPSHOT"
  )

lazy val sourceExamples = project.in(file("examples/sources"))

lazy val publishPluginLocal = taskKey[Unit]("Publish DSL plugin locally")
publishPluginLocal := {
  Command.process("clean", state.value)
  Command.process("compile", state.value)
  Command.process("package", state.value)
  Command.process("publishLocal", state.value)
}