organization := "de.tuda.stg"

name := "REScala"

version := "0.0.0"

scalaVersion := "2.10.3"

scalaSource in Compile <<= baseDirectory {(base) => new File(base, "REScala/src")}

scalaSource in Test <<= baseDirectory {(base) => new File(base, "REScalaTests/test")}

scalacOptions ++= List(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-target:jvm-1.6",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-Xlint",
  "-language:postfixOps"
)

// resolvers ++= Seq()

libraryDependencies ++= Seq(
  "org.mockito" % "mockito-all" % "1.9.5" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)
