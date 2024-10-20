/* This file is shared between multiple projects
 * and may contain unused dependencies */

// scalajs 1.0
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.17.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

// scalanative
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.5")

// crossbuilding
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")

// sbt settings
addSbtPlugin("com.github.sbt" % "sbt-dynver"    % "5.1.0") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.eed3si9n"   % "sbt-buildinfo" % "0.12.0")

// packaging
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.0")

// tooling
addSbtPlugin("pl.project13.scala" % "sbt-jmh"             % "0.4.7")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler" % "0.21.1")
//addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta44")
