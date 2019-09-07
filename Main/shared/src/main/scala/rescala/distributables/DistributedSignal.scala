package rescala.distributables

import loci.transmitter._
import rescala.default._
import rescala.macros.cutOutOfUserComputation
import rescala.lattices.Lattice

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
abstract class DistributedSignal[A, F: Lattice](initial: F, convert: F => A) {

  @cutOutOfUserComputation
  val crdtSignal: Var[F] = Var(initial)
  private[rescala] def merge(other: F): Unit = {
    crdtSignal.transform(Lattice[F].merge(_, other))
  }
  val valueSignal: Signal[A] = crdtSignal.map(convert)
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
                                          serializable: Serializable[Crdt],
                                          pVarFactory: PVarFactory[P]
                                         ): PushBasedTransmittable[P, Crdt, Crdt, Crdt, P] = {
    new PushBasedTransmittable[P, Crdt, Crdt, Crdt, P] {

      type From = Crdt
      type To = Crdt

      def send(value: P, remote: RemoteRef, endpoint: Endpoint[From, To]): To = {

        println(s"sending crdt $value")

        val observer = value.crdtSignal
                       .observe(c => endpoint.send(c))

        endpoint.receive notify value.merge

        endpoint.closed notify { _ => observer.remove }

        value.crdtSignal.readValueOnce
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]): P = {

        println(s"receiving crdt $value")


        val pVar = pVarFactory.create()

        pVar.merge(value)

        println(s"received $value")

        endpoint.receive notify { v =>
          println(s"received val: $value")
          pVar.merge(v)
        }
        val observer = pVar.crdtSignal.observe(c => endpoint.send(c))
        endpoint.closed notify { _ => observer.remove }

        // println(s"manual ${implicitly[StateCRDT[Int, GCounter]].merge(counter.crdtSignal.readValueOnce, value)}")


        pVar
      }
    }

  }

}
