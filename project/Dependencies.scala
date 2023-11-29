import sbt.*

object Dependencies {
  lazy val scala3Version = "3.3.1"
  lazy val scala3Compiler = "org.scala-lang" %% "scala3-compiler" % scala3Version % "provided"
}
