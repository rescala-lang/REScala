package rescala.crdts.pvars

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import rescala._
import rescala.crdts.pvars.DistributionEngine.{PublishReadOnly, PublishVar}
import rescala.crdts.statecrdts.StateCRDT

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

/**
  * Classes implementing this trait can be published and are then synchronized by the DistributionEngine (specified by
  * the implicit val `engine`). Internal changes to the underlying StateCRDT are made by firing an `internalChanges` event
  * containing the new StateCRDT. Similarly external changes recognized by the DistributionEngine fire an
  * `externalChanges` event.
  *
  * Methods allowing internal changes should be implemented by the implementing class.
  *
  * @tparam A The type of the underlying StateCRDT.
  */
trait Publishable[A <: StateCRDT] {
  lazy val changes: Event[A] = internalChanges || externalChanges
  lazy val crdtSignal: Signal[A] = changes.fold(initial) { (c1, c2) =>
    c1.merge(c2).asInstanceOf[A]
  }
  lazy val valueSignal: Signal[A#valueType] = crdtSignal.map(_.value)

  //val name: String
  val initial: A
  val internalChanges: Event[A]
  val externalChanges: Event[A]

  def get: A = now

  /**
    * Returns the current state of this publishable.
    *
    * @return a CRDT representing the current state
    */
  def now: A = crdtSignal.now

  /**
    * Shortcut to get the public value of the published CvRDT.
    * The actual return type depends on the chosen CvRDT. It could be Integer for Counter CRDTs or List for Sequence
    * based CvRDTs.
    *
    * @return an immutable object representing the public value of the published CvRDT
    */
  def value: A#valueType = valueSignal.now

  // TODO: implement blocking sync operation
  //def sync(implicit engine: ActorRef): Unit = engine ! SyncVar(this)

  // publish this to the distribution engine
  /*def publish(): Unit = {
    implicit val timeout = Timeout(60.second)
    val sendMessage = getEngine ? PublishVar(this)
    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }
*/
  // publish this to the distribution engine
  def publish(name: String)(implicit engine: ActorRef): Unit = {
    implicit val timeout = Timeout(60.second)
    val sendMessage = engine ? PublishVar(name, this)
    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }

  // publish this as read-only to the distribution engine
  def publishReadOnly(name: String)(implicit engine: ActorRef): Unit = {
    implicit val timeout = Timeout(60.second)
    val sendMessage = engine ? PublishReadOnly(name, this)
    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }
}
