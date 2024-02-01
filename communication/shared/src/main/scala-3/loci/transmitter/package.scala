package loci

package object transmitter:
  type Serializable[T] = serializer.Serializable[T]
  type Transmittable[B, I, R] = Transmittable.Resolution[B, I, R, ?, ? <: Transmittables]
