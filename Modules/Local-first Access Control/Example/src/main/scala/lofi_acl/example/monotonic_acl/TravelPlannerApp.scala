package lofi_acl.example.monotonic_acl

import scalafx.application.JFXApp3

object TravelPlannerApp extends JFXApp3 {
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = s"Travel Planner"
      scene = new MainScene()
      resizable = true
    }
  }

  override def stopApp(): Unit = {}
}
