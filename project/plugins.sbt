logLevel := Level.Warn

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.20")

addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.10.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.14")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.4.8")

addSbtPlugin("org.scala-native" % "sbt-crossproject"         % "0.1.0")  // (1)
addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.1.0")  // (2)
addSbtPlugin("org.scala-native" % "sbt-scala-native"         % "0.1.0")  // (3)
