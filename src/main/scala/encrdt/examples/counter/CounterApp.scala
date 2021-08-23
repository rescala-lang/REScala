package de.ckuessner
package encrdt.examples.counter

import javafx.{scene => jfxs}
import scalafx.Includes._
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.Scene
import scalafxml.core.{DependenciesByType, FXMLLoader}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object CounterApp extends JFXApp3 {
  val javaFxExecutionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor { cmd => Platform.runLater(cmd) }

  override def start(): Unit = {
    val loader = new FXMLLoader(getClass.getResource("CounterApp.fxml"), new DependenciesByType(Map.empty))
    loader.load()
    val root = loader.getRoot[jfxs.Parent]

    stage = new PrimaryStage() {
      title = "Counter App"
      scene = new Scene(root)
    }
  }
}

