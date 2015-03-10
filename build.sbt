organization := "de.tuda.stg"

name := "rescala"

version := "0.0.0"

scalaVersion := "2.11.2"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies ++= Seq(
  "org.mockito" % "mockito-all" % "1.9.5" % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test"
)

parallelExecution in Test := false

scalacOptions ++= (
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
