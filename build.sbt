name := "rescala-universe"

organization := "de.tuda.stg"

version := "0.3.0"

scalaVersion := "2.11.5"


lazy val root = Project(
  id = "rescala-universe",
  base = file("."),
  dependencies = List(rescala))

lazy val rescalaRoot = RootProject(file("../REScala"))

lazy val rescala = ProjectRef(rescalaRoot.build, "rescala")

scalaSource in Compile <<= baseDirectory { (base) => new File(base, "src") }

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies ++= (Nil)


scalacOptions ++= (
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

