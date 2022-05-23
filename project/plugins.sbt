/* This file is shared between multiple projects
 * and may contain unused dependencies */

// scalajs 1.0
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.20.0") // https://github.com/scalacenter/scalajs-bundler
addSbtPlugin("org.scala-js"  % "sbt-scalajs"         % "1.10.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

// scalanative
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.4")

// crossbuilding
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")

// operations
addSbtPlugin("com.dwijnand"     % "sbt-dynver"          % "4.1.1") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.8.1") // https://github.com/sbt/sbt-native-packager

// tooling
addSbtPlugin("org.tpolecat"       % "tut-plugin"  % "0.6.13")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"     % "0.4.3")
addSbtPlugin("org.irundaia.sbt"   % "sbt-sassify" % "1.5.1")
addSbtPlugin("com.github.sbt"     % "sbt-pgp"     % "2.1.2")

// https://github.com/scalameta/sbt-native-image
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.2")
