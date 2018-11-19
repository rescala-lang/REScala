logLevel := Level.Warn

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "0.6.0")
// addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.5.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.25")
// addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.3.7")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.9")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.12")

addSbtPlugin("com.dwijnand" % "sbt-dynver" % "3.1.0")
