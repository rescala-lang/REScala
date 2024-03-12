lazy val sourceExamples = project
  .in(file("."))
  .settings(
    name := "source-examples",
    scalaVersion := "3.3.1",

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    autoCompilerPlugins := true,
    addCompilerPlugin("de.tu-darmstadt.stud" %% "lore-dsl" % "0.0.1-SNAPSHOT"),
    libraryDependencies += "de.tu-darmstadt.stg" %% "lore" % "0.2-SNAPSHOT",

    publish / skip := true
  )
