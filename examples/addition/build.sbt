lazy val additionExample = project
  .in(file("."))
  .settings(
    name := "addition-example",
    scalaVersion := "3.3.1",

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    autoCompilerPlugins := true,
    addCompilerPlugin("de.tu-darmstadt.stud" %% "lore-dsl" % "0.0.1-SNAPSHOT"),

    publish / skip := true
  )
