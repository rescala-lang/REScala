name := "rescala"

organization := "de.tuda.stg"

version := "0.3.0"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.4", "2.11.2")

scalaSource in Compile <<= baseDirectory { (base) => new File(base, "REScala/src") }

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

scalaSource in Test <<= baseDirectory { (base) => new File(base, "REScala/test") }

parallelExecution in Test := false

excludeFilter <<= scalaVersion {
  case "2.10.4" => HiddenFileFilter || "*Macro*"
  case "2.11.2" => HiddenFileFilter
}

libraryDependencies ++= (
  "org.mockito" % "mockito-all" % "1.9.5" % "test" ::
    "org.scalatest" %% "scalatest" % "2.2.2" % "test" ::
    "com.novocode" % "junit-interface" % "0.11" % "test" ::
    Nil)


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

