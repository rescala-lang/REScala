package de.ckuessner
package encrdt.examples.counter


import encrdt.actors.{Counter, ObservableSynchronizationAdapter}
import encrdt.lattices.CounterCrdtLattice

import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Scheduler}
import akka.util.Timeout
import scalafx.scene.control.{Button, TextField, TextFormatter}
import scalafx.util.StringConverter
import scalafxml.core.macros.sfxml

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

@sfxml
class CounterController(private val addBtn: Button,
                        private val addField: TextField,
                        private val subBtn: Button,
                        private val subField: TextField,
                        private val countField: TextField) {

  val replicaId: String = UUID.randomUUID().toString

  val actorSystem: ActorSystem[Counter.Command] =
    ActorSystem(
      Behaviors.logMessages(
        Counter(replicaId, ctx =>
          new ObservableSynchronizationAdapter[CounterCrdtLattice](
            () => stateChanged(),
            ctx,
            replicaId,
            CounterCrdtLattice())
        )), "counterapp")

  Runtime.getRuntime.addShutdownHook(new Thread(() => actorSystem.terminate()))

  val counterActor: ActorRef[Counter.Command] = actorSystem

  addField.setTextFormatter(new TextFormatter(StringConverter(Integer.parseUnsignedInt, Integer.toString), 0))
  subField.setTextFormatter(new TextFormatter(StringConverter(Integer.parseUnsignedInt, Integer.toString), 0))

  addBtn.setOnAction { _ =>
    increment(Integer.parseUnsignedInt(addField.text.get()))
  }

  subBtn.setOnAction { _ =>
    decrement(Integer.parseUnsignedInt(subField.text.get()))
  }

  def stateChanged(): Unit = {
    Console.println("CounterController::stateChanged()")
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
}