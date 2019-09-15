package rescala.extra.distributables

import loci.transmitter._
import rescala.default._
import rescala.macros.cutOutOfUserComputation
import rescala.extra.lattices.Lattice


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


  implicit def DistributedSignalTransmittable
  [P, Crdt, T, I](implicit
                     ev: P <:< DistributedSignal[_, Crdt],
                     pVarFactory: PVarFactory[P],
                     transmittable: Transmittable[Crdt, I, Crdt])
  : ConnectedTransmittable[P, I, P] {
    type Message = transmittable.Type
  } = {

    ConnectedTransmittable(
      provide = (value, context) => {
        val observer = value.crdtSignal.observe(context.endpoint.send)
        context.endpoint.closed notify { _ => observer.remove }
        value.crdtSignal.readValueOnce
      },
      receive = (value, context) => {
        val signal = pVarFactory.create()
        signal.merge(value)
        context.endpoint.receive.notify { signal.merge }
        signal
      })
  }

}

