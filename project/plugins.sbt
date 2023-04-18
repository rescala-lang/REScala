/* This file is shared between multiple projects
 * and may contain unused dependencies */

// scalajs 1.0
addSbtPlugin("org.scala-js"            % "sbt-scalajs"              % "1.13.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

// scalanative
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.12")

// crossbuilding
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")

// sbt settings
addSbtPlugin("com.dwijnand" % "sbt-dynver"    % "4.1.1") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

// packaging
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")

// tooling
addSbtPlugin("pl.project13.scala"          % "sbt-jmh"       % "0.4.4")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta41")
