lazy val tcp = project.in(file("communicator-tcp")).settings(
  Settings.scalaVersion_3,
  Dependencies.slips.delay
)
