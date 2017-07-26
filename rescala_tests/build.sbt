name := "rescala_tests"

version := "1.0"

scalaVersion := "2.12.1"

val akkaVersion = "2.5.3"

resolvers += Resolver.bintrayRepo("rmgk", "maven")
resolvers += Resolver.bintrayRepo("pweisenburger", "maven")

libraryDependencies += "de.tuda.stg" %% "rescala" % "0.19.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.3",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.3" % Test,
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-metrics" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
  "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion
)

