name := "paroli-chat"

version := "1.0"

scalaVersion := "2.12.1"

val akkaVersion = "2.5.3"

// rescala:
resolvers += Resolver.bintrayRepo("rmgk", "maven")
resolvers += Resolver.bintrayRepo("pweisenburger", "maven")
libraryDependencies += "de.tuda.stg" %% "rescala" % "0.19.0"

// akka:
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-slf4j" % "2.5.3",
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-metrics" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
  "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion)

// console input:
libraryDependencies += "org.scala-lang.modules" % "scala-jline" % "2.12.1"

// logging:
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"