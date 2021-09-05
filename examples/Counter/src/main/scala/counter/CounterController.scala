package de.ckuessner
package counter

import counter.CounterApp.syncServiceKey
import counter.actors.{Counter, ObservableSynchronizationAdapter}
import encrdt.lattices.CounterLattice

import akka.actor.AddressFromURIString
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Scheduler}
import akka.cluster.typed.{Cluster, JoinSeedNodes}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafxml.core.macros.sfxml

import java.net.MalformedURLException
import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

trait CounterControllerInterface {
  def stop(): Unit

  val replicaId: String

  def increment(delta: Int): Unit

  def decrement(delta: Int): Unit
}

@sfxml
class CounterController(private val addBtn: Button,
                        private val subBtn: Button,
                        private val deltaSpinner: Spinner[Int],
                        private val countField: TextField,
                        private val connectBtn: Button,
                        private val addressTextField: TextField)
  extends CounterControllerInterface {

  val replicaId: String = UUID.randomUUID().toString

  private val actorSystem: ActorSystem[Counter.Command] = ActorSystem.create(
    Behaviors.logMessages(
      Counter(replicaId, ctx =>
        new ObservableSynchronizationAdapter[CounterLattice](
          () => stateChanged(),
          ctx,
          syncServiceKey,
          replicaId,
          CounterLattice()),
        syncServiceKey
      )),
    "Counter",
    ConfigFactory.load("application.conf")
  )

  addressTextField.setText(actorSystem.address.toString)

  private val counterActor: ActorRef[Counter.Command] = actorSystem

  private val cluster: Cluster = Cluster(actorSystem)

  addBtn.setOnAction { _ =>
    increment(deltaSpinner.value.get())
  }

  subBtn.setOnAction { _ =>
    decrement(deltaSpinner.value.get())
  }

  connectBtn.setOnAction { _ =>
    val dialog = new TextInputDialog() {
      initOwner(CounterApp.stage)
      title = "Connect to seed node"
      headerText = "Connect to seed node"
      contentText = "URI of seed node:"
    }
    dialog.showAndWait() match {
      case Some(addressString) =>
        try {
          if (!addressString.isBlank) {
            val address = AddressFromURIString.parse(addressString)
            cluster.manager ! JoinSeedNodes(Seq(address))
          }
        } catch {
          case urle: MalformedURLException => new Alert(AlertType.Error) {
            title = "Malformed URI"
            headerText = "Malformed URI error"
            contentText = "Not a valid URI: " + urle.getMessage
          }.show()
        }
      case None =>
    }
  }

  def stateChanged(): Unit = {
    val timeout: Timeout = 1.seconds
    // Use Akka ActorSystem scheduler for ask
    val scheduler: Scheduler = actorSystem.scheduler
    val future: Future[Counter.Value] = counterActor.ask(Counter.Query)(timeout, scheduler)

    future.onComplete {
      case Failure(exception) => Console.err.println(exception)
      case Success(Counter.Value(count)) => onCountChanged(count)
    }(CounterApp.javaFxExecutionContext) // Use ExecutionContext of JavaFX for onCountChanged(...)
  }

  private def onCountChanged(newCount: Int): Unit = {
    countField.setText(newCount.toString)
  }

  def increment(delta: Int): Unit = {
    counterActor ! Counter.Update(delta)
  }

  def decrement(delta: Int): Unit = {
    counterActor ! Counter.Update(-delta)
  }

  def stop(): Unit = {
    actorSystem.terminate()
  }
}