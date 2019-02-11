package rescala.crdts.distributables

import loci.transmitter._
import rescala.crdts.statecrdts.StateCRDT
import rescala.default._
import rescala.macros.cutOutOfUserComputation

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
abstract class DistributedSignal[A, F](initial: F)(implicit stateCRDT: StateCRDT[A, F]) {
  type valueType = A
  type crdtType = F

  @cutOutOfUserComputation
  private[rescala] val crdtSignal       : Var[F]    = Var(initial)
  private[rescala] val localDeviceChange: Evt[Unit] = Evt[Unit]
  private[rescala] def mergeInternal(other: F) = {
    crdtSignal.transform(stateCRDT.merge(_, other))
  }
  def merge(other: F) = {
    mergeInternal(other)
    localDeviceChange.fire()
  }
  val valueSignal: Signal[A] = crdtSignal.map(s => stateCRDT.value(s))

  /**
    * Shortcut to get the public value of the published CvRDT.
    * The actual return type depends on the chosen CvRDT. It could be Integer for Counter CRDTs or List for Sequence
    * based CvRDTs.
    *
    * @return an immutable object representing the public value of the published CvRDT
    */
  def value: A = valueSignal.readValueOnce

}

object DistributedSignal {

  trait PVarFactory[A] {
    def apply(): A

    def create(): A = apply()
  }

  /**
    * @tparam Crdt CRDT type
    * @tparam P    pVar type
    **/
  implicit def PVarTransmittable[Crdt, P](implicit
                                          ev: P <:< DistributedSignal[_, Crdt],
                                          transmittable: Transmittable[Crdt, Crdt, Crdt],
                                          serializable : Serializable[Crdt],
                                          pVarFactory  : PVarFactory[P]
                                         ): PushBasedTransmittable[P, Crdt, Crdt, Crdt, P] = {
    new PushBasedTransmittable[P, Crdt, Crdt, Crdt, P] {

      type From = Crdt
      type To = Crdt

      def send(value: P, remote: RemoteRef, endpoint: Endpoint[From, To]): To = {

        println(s"sending crdt $value")

        val observer = value.crdtSignal
                       .observe(c => endpoint.send(c))

        endpoint.receive notify value.mergeInternal

        endpoint.closed notify { _ => observer.remove }

        value.crdtSignal.readValueOnce
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]): P = {

        println(s"receiving crdt $value")


        val pVar = pVarFactory.create()

        pVar.merge(value)

        println(s"received $value")
        println(s"before: ${pVar.value}, ")

        endpoint.receive notify { v =>
          println(s"received val: $value")
          pVar.mergeInternal(v)
        }
        val observer = pVar.crdtSignal.observe(c => endpoint.send(c))
        endpoint.closed notify { _ => observer.remove }

        // println(s"manual ${implicitly[StateCRDT[Int, GCounter]].merge(counter.crdtSignal.readValueOnce, value)}")

        println(s"after: ${pVar.value}")

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
