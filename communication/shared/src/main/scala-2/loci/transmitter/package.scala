package loci

package object transmitter {
  type Serializable[T] = serializer.Serializable[T]

  type Transmittable[B, I, R] = TransmittableBase.Wrapper[B, I, R, _, _ <: Transmittables]

  object Transmittable {
    type Base = TransmittableBase.type

    type Aux[-B, I, +R, P, T <: Transmittables] = TransmittableBase.Any[B, I, R] {
      type Proxy = P
      type Transmittables = T
    }

    type Any[-B, I, +R] = TransmittableBase.Any[B, I, R]

    type Resolution[B, I, R, P, T <: Transmittables] = TransmittableBase.Resolution[B, I, R, P, T]

    type Delegating = TransmittableBase.Delegating

    object Delegating {
      type Resolution[D <: Delegating] = TransmittableBase.Delegating.Resolution[D]
    }

    @inline def apply[T](implicit resolution: Resolution[T, _, _, _, _]) =
      resolution.transmittable

    @inline def Argument[T](implicit resolution: Resolution[T, _, T, _, _]) =
      resolution.transmittable
  }
}
