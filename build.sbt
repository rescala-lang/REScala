scalaVersion := "2.11.8"

name := "rescalawebapps"
resolvers += Resolver.bintrayRepo("rmgk", "maven")

enablePlugins(ScalaJSPlugin)
libraryDependencies += "de.tuda.stg" %% "rescala" % "0.18.0"
libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.18.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

scalaSource in Compile := baseDirectory.value
