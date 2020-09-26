scalaVersion := "2.12.6"
resolvers += Resolver.bintrayRepo("stg-tud", "maven")
libraryDependencies += "de.tuda.stg" %% "rescala" % "0.24.0"
libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5", "org.slf4j" % "slf4j-simple" % "1.7.5")
enablePlugins(JmhPlugin)
