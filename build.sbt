enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

lazy val rescalatags = ProjectRef(file("../REScala"), "rescalatags")

dependsOn(rescalatags)

name := "daimpl"
scalaVersion := "2.12.2"
scalaSource in Compile := baseDirectory.value
