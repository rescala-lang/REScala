lazy val tcp = project.in(file("communicator-tcp")).settings(
  Settings.scalaVersion_3,
  Dependencies.munit
).dependsOn(interfaces.jvm)

lazy val nativewebsockets = project.in(file("communicator-ws-webnative"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Settings.scalaVersion_3,
    Dependencies.munit,
    Dependencies.scalajsDom,
  ).dependsOn(interfaces.js)

lazy val interfaces = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("interfaces")).settings(
    Settings.scalaVersion_3,
    Dependencies.slips.delay,
  )
