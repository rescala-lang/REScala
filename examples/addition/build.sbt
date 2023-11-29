import Dependencies.*

lazy val additionExample = project
  .in(file("."))
  .settings(
    name := "addition-example",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    autoCompilerPlugins := true,
    addCompilerPlugin("lore.DSL" %% "lore-dsl" % "0.0.1-SNAPSHOT"),

    publish / skip := true
  )
