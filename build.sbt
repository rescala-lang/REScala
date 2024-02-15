import sbt.librarymanagement.Configurations.TestInternal

lazy val channelsjvm = project.in(file("channels-jvm")).settings(
  Settings.scala3defaults,
  Dependencies.munit
).dependsOn(interfaces.jvm)

lazy val channelsweb = project.in(file("channels-web"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Settings.scala3defaults,
    Dependencies.munit,
    Dependencies.scalajsDom,
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  ).dependsOn(interfaces.js)


lazy val integrationweb = project.in(file("integration-web"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Settings.scala3defaults,
    Dependencies.munit,
    Dependencies.scalajsDom,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  ).dependsOn(interfaces.js, channelsweb)

lazy val interfaces = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("interfaces")).settings(
    Settings.scala3defaults,
    Dependencies.slips.delay,
  )

lazy val channelsjetty = project.in(file("channels-jetty")).settings(
  Settings.scala3defaults,
  Dependencies.munit,
  libraryDependencies ++= {
    val jettyVersion = "12.0.6"
    Seq(
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
      "org.slf4j"                   % "slf4j-nop"                    % "2.0.12" % TestInternal
    )
  }
).dependsOn(interfaces.jvm)
