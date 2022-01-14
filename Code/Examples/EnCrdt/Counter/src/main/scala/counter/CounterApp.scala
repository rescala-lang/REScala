package de.ckuessner
package counter

import counter.actors.SynchronizationAdapter
import encrdt.lattices.CounterLattice

import akka.actor.typed.receptionist.ServiceKey
import javafx.{scene => jfxs}
import scalafx.Includes._
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.Scene
import scalafxml.core.{DependenciesByType, FXMLLoader}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object CounterApp extends JFXApp3 {
  val javaFxExecutionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor { cmd => Platform.runLater(cmd) }

  val syncServiceKey: ServiceKey[SynchronizationAdapter.Command[CounterLattice]] = ServiceKey("counterSync")

  private var controller: CounterControllerInterface = _

  override def start(): Unit = {
    val loader = new FXMLLoader(getClass.getResource("CounterApp.fxml"), new DependenciesByType(Map.empty))
    loader.load()
    val root = loader.getRoot[jfxs.Parent]
    controller = loader.getController()

    stage = new PrimaryStage() {
      title = "Counter App"
      scene = new Scene(root)
    }
  }

  override def stopApp(): Unit = {
    controller.stop()
    super.stopApp()
  }
}

