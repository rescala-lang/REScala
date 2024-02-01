package loci
package transmitter

import scala.collection.IterableOps

trait TransmittableGeneralIterableCollections extends TransmittableDummy {
  this: Transmittable.Base =>

  final implicit def iterable[B, I, R, V[T] <: IterableOps[T, V, V[T]]]
    (implicit transmittable: Transmittable[B, I, R])
  : DelegatingTransmittable[V[B], V[I], V[R]] {
      type Delegates = transmittable.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null.asInstanceOf[V[I]] else value map { context delegate _ },
      receive = (value, context) =>
        if (value == null) null.asInstanceOf[V[R]] else value map { context delegate _ })
}

trait TransmittableIterableCollections extends TransmittableGeneralCollections {
  this: Transmittable.Base =>

  final implicit def identicalIterable
    [T: IdenticallyTransmittable, V[T] <: IterableOps[T, V, V[T]]]
  : IdenticallyTransmittable[V[T]] = IdenticallyTransmittable()
}
