package rescala.crdts.pvars

import akka.actor.ActorRef
import rescala._
import rescala.crdts.statecrdts.StateCRDT

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
abstract class Publishable[A, F]()(implicit stateCRDT: StateCRDT[A, F]) {
  lazy val changes: Event[F] = internalChanges || externalChanges
  lazy val crdtSignal: Signal[F] = changes.fold(initial) { (c1, c2) =>
    stateCRDT.merge(c1,c2)
  }
  lazy val valueSignal: Signal[A] = crdtSignal.map(s => stateCRDT.value(s))

  //val name: String
  val initial: F
  val internalChanges: Event[F]
  val externalChanges: Event[F]

  /**
    * Shortcut to get the public value of the published CvRDT.
    * The actual return type depends on the chosen CvRDT. It could be Integer for Counter CRDTs or List for Sequence
    * based CvRDTs.
    *
    * @return an immutable object representing the public value of the published CvRDT
    */
  def value: A = valueSignal.readValueOnce

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
    locally(name); locally(engine)
//    implicit val timeout = Timeout(60.second)
//    val sendMessage = engine ? PublishVar(name, this)
//    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }

  // publish this as read-only to the distribution engine
  def publishReadOnly(name: String)(implicit engine: ActorRef): Unit = {
    locally(name); locally(engine)
//    implicit val timeout = Timeout(60.second)
//    val sendMessage = engine ? PublishReadOnly(name, this)
//    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }
}
