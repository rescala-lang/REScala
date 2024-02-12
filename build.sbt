import sbt.librarymanagement.Configurations.TestInternal

lazy val tcp = project.in(file("communicator-tcp")).settings(
  Settings.scala3defaults,
  Dependencies.munit
).dependsOn(interfaces.jvm)

lazy val nativewebsockets = project.in(file("communicator-ws-webnative"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Settings.scala3defaults,
    Dependencies.munit,
    Dependencies.scalajsDom,
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  ).dependsOn(interfaces.js)

lazy val interfaces = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("interfaces")).settings(
    Settings.scala3defaults,
    Dependencies.slips.delay,
  )

lazy val jetty12 = project.in(file("communicator-ws-jetty12")).settings(
  Settings.scala3defaults,
  Dependencies.munit,
  libraryDependencies ++= {
    val jettyVersion = "12.0.5"
    Seq(
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
      "org.slf4j"                   % "slf4j-nop"                    % "2.0.11" % TestInternal
    )
  }
).dependsOn(interfaces.jvm)
