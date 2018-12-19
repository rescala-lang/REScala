package rescala.crdts.pvars

import akka.actor.ActorRef
import loci.transmitter._
import rescala.default._
import rescala.crdts.statecrdts.StateCRDT

/**
  * Classes implementing this trait can be published and are then synchronized by the DistributionEngine (specified by
  * the implicit val `engine`). Internal changes to the underlying StateCRDT are made by firing an `internalChanges` event
  * containing the new StateCRDT. Similarly external changes recognized by the DistributionEngine fire an
  * `externalChanges` event.
  *
  * Methods allowing internal changes should be implemented by the implementing class.
  *
  * @tparam A The value type of the underlying StateCRDT.
  */
abstract class Publishable[A, F]()(implicit stateCRDT: StateCRDT[A, F]) {
  type valueType = A
  type crdtType = F

  lazy val changes: Event[F] = internalChanges || externalChanges
  lazy val crdtSignal: Signal[F] = changes.fold(initial) { (c1, c2) =>
    stateCRDT.merge(c1, c2)
  }
  lazy val valueSignal: Signal[A] = crdtSignal.map(s => stateCRDT.value(s))

  //val name: String
  val initial: F
  val internalChanges: Evt[F]
  val externalChanges: Evt[F]

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
    locally(name);
    locally(engine)
    //    implicit val timeout = Timeout(60.second)
    //    val sendMessage = engine ? PublishVar(name, this)
    //    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }

  // publish this as read-only to the distribution engine
  def publishReadOnly(name: String)(implicit engine: ActorRef): Unit = {
    locally(name)
    locally(engine)
    //    implicit val timeout = Timeout(60.second)
    //    val sendMessage = engine ? PublishReadOnly(name, this)
    //    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
  }
}

object Publishable {

  trait PVarFactory[A] {
    def apply(): A

    def create(): A = apply()
  }

  /**
    * @tparam Crdt CRDT type
    * @tparam P    pVar type
    **/
  implicit def PVarTransmittable[Crdt, P](implicit
                                          ev: P <:< Publishable[_, Crdt],
                                          transmittable: Transmittable[Crdt, Crdt, Crdt],
                                          serializable: Serializable[Crdt],
                                          pVarFactory: PVarFactory[P]
                                         ): PushBasedTransmittable[P, Crdt, Crdt, Crdt, P] = {
    new PushBasedTransmittable[P, Crdt, Crdt, Crdt, P] {

      type From = Crdt
      type To = Crdt

      def send(value: P, remote: RemoteRef, endpoint: Endpoint[From, To]): To = {

        val observer = value.internalChanges.observe(c => endpoint.send(c))

        endpoint.receive notify value.externalChanges.fire

        endpoint.closed notify { _ => observer.remove }

        value.crdtSignal.readValueOnce
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]): P = {
        val pVar = pVarFactory.create()
        locally(pVar.valueSignal)
        pVar.externalChanges fire value

        println(s"received $value")
        println(s"before: $pVar, ")

        endpoint.receive notify pVar.externalChanges.fire
        val observer = pVar.internalChanges.observe(c => endpoint.send(c))
        endpoint.closed notify { _ => observer.remove }

        // println(s"manual ${implicitly[StateCRDT[Int, GCounter]].merge(counter.crdtSignal.readValueOnce, value)}")

        println(s"after: $pVar")

        pVar
      }
    }

  }

}

/*
// A: value type
// F: crdt type
// P: publishable type
// this is needed to make pVars transmittable using the loci framework
implicit def rescalaSignalTransmittable[F,S](implicit
                                           transmittable: Transmittable[F, S, F],
                                           serializable: Serializable[S], pVarFactory: PVarFactory[A,F]) = {
  type From = F
  type To = F

  new PushBasedTransmittable[Publishable[_,F], From, S, To, Publishable[_,F]] {


    def send(value: Publishable[_,F], remote: RemoteRef, endpoint: Endpoint[From, To]): To = {

      val observer = value.internalChanges.observe(c => endpoint.send(c))

      endpoint.receive notify value.externalChanges.fire

      endpoint.closed notify { _ => observer.remove }

      value.crdtSignal.readValueOnce
    }

    def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]): Publishable[_,F] = {
      val pvar : Publishable[_,F] = createPVar[To]
      locally(pvar.valueSignal)
      pvar.externalChanges.fire(value)

      println(s"received $value")
      println(s"before: $pvar, ")

      endpoint.receive notify pvar.externalChanges.fire
      val observer = pvar.internalChanges.observe(c => endpoint.send(c))
      endpoint.closed notify { _ => observer.remove }

      // println(s"manual ${implicitly[StateCRDT[Int, GCounter]].merge(counter.crdtSignal.readValueOnce, value)}")

      println(s"after: $pvar")

      pvar
    }
  }
}
*/
