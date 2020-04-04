/** This file is shared between multiple projects
  * and may contain unused dependencies */

// scalajs
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.17.0") // https://github.com/scalacenter/scalajs-bundler
addSbtPlugin("org.irundaia.sbt" % "sbt-sassify" % "1.4.13")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.1")
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0"

// operations
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.0.0") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.0") // https://github.com/sbt/sbt-native-packager

// tooling
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.13")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")

// bloop for metals and intellij bsp
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.3.5")
