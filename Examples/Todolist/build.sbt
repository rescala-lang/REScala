enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

libraryDependencies += "de.tuda.stg" %%% "rescala" % "0.20.0-SNAPSHOT"
libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.20.0-SNAPSHOT"

name := "daimpl"
scalaVersion := "2.12.2"
scalaSource in Compile := baseDirectory.value
