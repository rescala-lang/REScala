import sbt.librarymanagement.Configurations.TestInternal

lazy val channels = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("channels")).settings(
    Settings.scala3defaults,
    Dependencies.slips.delay,
    Dependencies.munit,
    Dependencies.jsoniterScala,
  ).jsSettings(
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),
    Dependencies.scalajsDom,
    Dependencies.scalatags,
  ).jvmSettings(
    libraryDependencies ++= {
      val jettyVersion = "12.0.6"
      Seq(
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
        "org.slf4j"                   % "slf4j-nop"                    % "2.0.12" % TestInternal
      )
    }
  )
