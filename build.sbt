name := "rescala"

organization := "de.tuda.stg"

version := "0.4.0"

scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.10.4", "2.11.4")

scalaSource in Compile <<= baseDirectory { (base) => new File(base, "REScala/src") }

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

scalaSource in Test <<= baseDirectory { (base) => new File(base, "REScala/test") }

parallelExecution in Test := true

excludeFilter <<= scalaVersion {
  case s if s.startsWith("2.10.") => HiddenFileFilter || "*Macro*"
  case s if s.startsWith("2.11.") => HiddenFileFilter
}

libraryDependencies ++= (
  "org.mockito" % "mockito-all" % "1.10.8" % "test" ::
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

