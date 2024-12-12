package ex2024travel.lofi_acl.example.monotonic_acl

import scalafx.application.{JFXApp3, Platform}

object TravelPlannerApp extends JFXApp3 {
  override def start(): Unit = {
    Platform.implicitExit = true
    stage = new JFXApp3.PrimaryStage {
      title = s"Travel Planner"
      scene = new MainScene()
      resizable = true
    }
  }

  override def stopApp(): Unit = {
    System.exit(0) // Workaround to ensure that Runtime shutdown hooks are executed
  }
}
