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
