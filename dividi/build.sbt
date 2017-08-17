name := "dividi"

version := "1.0"

scalaVersion := "2.12.1"

val akkaVersion = "2.5.3"

// rescala:
resolvers += Resolver.bintrayRepo("rmgk", "maven")
resolvers += Resolver.bintrayRepo("pweisenburger", "maven")
libraryDependencies += "de.tuda.stg" %% "rescala" % "0.19.0"

// scalafx
libraryDependencies += "org.scalafx" % "scalafx_2.12" % "8.0.102-R11"
libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0"

// akka:
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-metrics" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
  "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion)

// logging:
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"