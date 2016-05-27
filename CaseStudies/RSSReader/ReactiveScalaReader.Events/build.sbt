name := "ReactiveReader"

version := "1.0"

scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test")

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")

scalacOptions += "-P:continuations:enable"

scalacOptions ++= Seq("-deprecation","-unchecked")

libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.0" withSources(),
    "org.joda" % "joda-convert" % "1.2",
    "org.codehaus.jsr166-mirror" % "jsr166y" % "1.7.0",
    "org.scalatest" %% "scalatest" % "1.8" % "test",
    "junit" % "junit" % "4.10" % "test->default",
    "com.novocode" % "junit-interface" % "0.8" % "test->default",
    "org.scala-lang" % "scala-swing" % "2.9.2"
)

seq(ScctPlugin.instrumentSettings : _*)
