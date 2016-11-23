enablePlugins(ScalaJSPlugin)
resolvers += Resolver.bintrayRepo("rmgk", "maven")

libraryDependencies += "de.tuda.stg" %% "rescala" % "0.18.0"
libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.18.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

name := "daimpl"
scalaVersion := "2.11.8"
scalaSource in Compile := baseDirectory.value
