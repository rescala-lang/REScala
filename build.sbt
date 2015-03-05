
lazy val root = Project("rescala", file("."))
  .settings(
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    parallelExecution in Test := false,
    libraryDependencies ++= (
      "org.mockito" % "mockito-all" % "1.9.5" % "test" ::
      "org.scalatest" %% "scalatest" % "2.2.1" % "test" ::
      "com.novocode" % "junit-interface" % "0.10" % "test" ::
      Nil)
  )

//lazy val tests = Project("rescala-tests", file("REScalaTests"))
//  .settings(
//    scalaSource in Test <<= baseDirectory {(base) => new File(base, "test")},
//    parallelExecution in Test := false,
//    libraryDependencies ++= (
//      "org.mockito" % "mockito-all" % "1.9.5" % "test" ::
//      "org.scalatest" %% "scalatest" % "2.2.1" % "test" ::
//      "com.novocode" % "junit-interface" % "0.10" % "test" ::
//      Nil)
//  )
//  .dependsOn(rescala)

organization in ThisBuild := "de.tuda.stg"

version in ThisBuild := "0.0.0"

scalaVersion in ThisBuild := "2.11.2"

scalacOptions in ThisBuild ++= (
  "-deprecation" ::
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-feature" ::
  "-target:jvm-1.6" ::
  //"-language:implicitConversions" ::
  //"-language:reflectiveCalls" ::
  "-Xlint" ::
  //"-language:postfixOps" ::
  Nil)

