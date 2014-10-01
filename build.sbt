lazy val root = Project("root", file(".")).aggregate(core, tests)

lazy val core = Project("rescala", file("REScala"))
  .settings(
    scalaSource in Compile <<= baseDirectory {(base) => new File(base, "src")},
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _))

lazy val tests = Project("tests", file("REScalaTests"))
  .settings(
    scalaSource in Test <<= baseDirectory {(base) => new File(base, "test")},
    parallelExecution in Test := false,
    libraryDependencies ++= (
      "org.mockito" % "mockito-all" % "1.9.5" % "test" ::
      "org.scalatest" %% "scalatest" % "2.2.1" % "test" ::
      "com.novocode" % "junit-interface" % "0.10" % "test" ::
      Nil)
  )
  .dependsOn(core)

organization in ThisBuild := "de.tuda.stg"

version in ThisBuild := "0.0.0"

scalaVersion in ThisBuild := "2.11.2"

scalacOptions in ThisBuild ++= (
  "-deprecation" ::
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-feature" ::
  "-target:jvm-1.7" ::
  "-Xlint" ::
  "-Xfuture" ::
  //"-Xlog-implicits" ::
  //"-Yno-predef" ::
  //"-Yno-imports" ::
  "-Xfatal-warnings" ::
  "-Yinline-warnings" ::
  "-Yno-adapted-args" ::
  "-Ywarn-dead-code" ::
  "-Ywarn-nullary-override" ::
  "-Ywarn-nullary-unit" ::
  //"-Ywarn-numeric-widen" ::
  //"-Ywarn-value-discard" ::
  Nil)

