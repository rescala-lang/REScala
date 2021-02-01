/** This file is shared between multiple projects
  * and may contain unused dependencies
  */

// scalajs 0.6
// addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler-sjs06" % "0.17.0") // https://github.com/scalacenter/scalajs-bundler
// addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")
// addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.32")

// scalajs 1.0
addSbtPlugin("ch.epfl.scala"           % "sbt-scalajs-bundler"      % "0.20.0") // https://github.com/scalacenter/scalajs-bundler
addSbtPlugin("org.portable-scala"      % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js"            % "sbt-scalajs"              % "1.4.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

// scalanative
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.0")

// crossbuilding
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.0.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.0.0")

addSbtPlugin("org.irundaia.sbt" % "sbt-sassify" % "1.5.1")

// operations
addSbtPlugin("com.dwijnand"     % "sbt-dynver"          % "4.1.1") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.6") // https://github.com/sbt/sbt-native-packager

// tooling
addSbtPlugin("org.tpolecat"       % "tut-plugin" % "0.6.13")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"    % "0.4.0")

// dotty?
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.5.2")

// https://github.com/scalameta/sbt-native-image
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.0")
