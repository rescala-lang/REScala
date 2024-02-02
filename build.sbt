lazy val tcp = project.in(file("communicator-tcp")).settings(
  Settings.scalaVersion_3,
  Dependencies.slips.delay,
  Dependencies.munit
)

lazy val nativewebsockets = project.in(file("communicator-ws-webnative"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Settings.scalaVersion_3,
    Dependencies.slips.delay,
    Dependencies.munit
  )
