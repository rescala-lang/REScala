/** This file is shared between multiple projects
  * and may contain unused dependencies */

// scalajs
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler-sjs06" % "0.16.0") // https://github.com/scalacenter/scalajs-bundler
addSbtPlugin("org.irundaia.sbt" % "sbt-sassify" % "1.4.13")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.31")

// operations
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.0.0") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.5.2") // https://github.com/sbt/sbt-native-packager

// tooling
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.13")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")

// bloop for metals and intellij bsp
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.3.5")
