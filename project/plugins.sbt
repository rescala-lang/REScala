/** This file is shared between multiple projects
  * and may contain unused dependencies */

// scalajs
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.14.0")
addSbtPlugin("org.irundaia.sbt" % "sbt-sassify" % "1.4.12")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.26")

// operations
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "3.3.0") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.19") // https://github.com/sbt/sbt-native-packager

// tooling
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.10")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")

