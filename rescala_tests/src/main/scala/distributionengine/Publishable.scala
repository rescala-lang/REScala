package distributionengine

import akka.actor.ActorRef
import akka.util.Timeout
import rescala.parrp.ParRP
import rescala.{Event, Signal, reactives}
import statecrdts.StateCRDT
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import rescala._
import rescala.parrp.ParRP
import statecrdts.{CIncOnlyCounter, RGA, StateCRDT, Vertex}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Classes implementing this trait can be published and are then synchronized by the DistributionEngine `engine`.
  * Internal changes to the underlying StateCRDT are made by firing an internalChanges event containing the new
  * StateCRDT. Similarly external changes recognized by the DistributionEngine fire an `externalChanges` event.
  *
  * Methods allowing internal changes must be implemented by the implementing class. Otherwise it is impossible to fire
  * `internalChanges` events.
  *
  * @tparam A The type of the underlying StateCRDT.
  */
trait Publishable[A <: StateCRDT] {
  val engine: ActorRef
  val name: String
  val initial: A
  protected val internalChanges: Event[A] // protected to prevent firing of internalChanges events outside the implementing class
  val externalChanges: Event[A]
  lazy val changes: Event[A] = internalChanges || externalChanges
  lazy val signal: Signal[A] = changes.fold(initial) { (c1, c2) =>
    c1.merge(c2) match {
      case a: A => a
    }
  }

  /**
    * Returns an immutable object representing the public value of the published CvRDT.
    * The actual return type depends on the chosen CvRDT. It could be Integer for Counter CRDTs or List for Sequence
    * based CvRDTs.
    *
    * @return an immutable object representing the public value of the published CvRDT
    */
  def getValue: A#valueType = signal.now.value

  def sync(): Unit = engine ! SyncVar(this)

  // publish this to the distribution engine
  def publish(): Unit = {
    implicit val timeout = Timeout(60.second)
    val sendMessage = engine ? PublishVar(this)
    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }
}