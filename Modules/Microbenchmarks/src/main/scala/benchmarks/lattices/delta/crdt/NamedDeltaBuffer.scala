package benchmarks.lattices.delta.crdt

import rdts.base.{Decompose, Lattice, LocalUid, Uid}
import rdts.dotted.{Dotted, HasDots, Obrem}
import rdts.time.{Dot, Dots}

type DeltaBufferDotted[State] = NamedDeltaBuffer[Dotted[State]]

case class Named[T](replicaId: Uid, anon: T)

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class NamedDeltaBuffer[State](
    replicaID: LocalUid,
    state: State,
    deltaBuffer: List[Named[State]] = Nil
) {

  inline def map(f: LocalUid ?=> State => State)(using Lattice[State], Decompose[State]): NamedDeltaBuffer[State] =
    applyDelta(replicaID.uid, f(using replicaID)(state))

  def applyDelta(source: Uid, delta: State)(using Lattice[State], Decompose[State]): NamedDeltaBuffer[State] =
    Lattice[State].diff(state, delta) match {
      case Some(stateDiff) =>
        val stateMerged = Lattice[State].merge(state, stateDiff)
        new NamedDeltaBuffer(replicaID, stateMerged, Named(source, stateDiff) :: deltaBuffer)
      case None => this
    }

  def clearDeltas(): NamedDeltaBuffer[State] = copy(deltaBuffer = List())

  def transform(f: State => State)(using Lattice[State], Decompose[State]) = applyDelta(replicaID.uid, f(state))
}

object NamedDeltaBuffer {

  extension [A](curr: DeltaBufferDotted[A])(using Lattice[Dotted[A]], Decompose[Dotted[A]])
    inline def mod(f: Dots ?=> A => Dotted[A]): DeltaBufferDotted[A] = {
      curr.applyDelta(curr.replicaID.uid, curr.state.mod(f(_)))
    }
  extension [A](curr: DeltaBufferDotted[A]) def data: A = curr.state.data

  implicit object workaround {
    extension [A](curr: NamedDeltaBuffer[A])(using Lattice[A],  Decompose[A])
      inline def mod(f: A => A): NamedDeltaBuffer[A] = {
        curr.applyDelta(curr.replicaID.uid, f(curr.state))
      }
  }

  implicit object workaround2 {
    extension [A](curr: NamedDeltaBuffer[Obrem[A]])(using Lattice[Obrem[A]])
      inline def mod(f: Dots ?=> A => Obrem[A]): NamedDeltaBuffer[Obrem[A]] = {
        given Decompose[Obrem[A]] = Decompose.atomic
        curr.applyDelta(curr.replicaID.uid, curr.state.mod(f(_)))
      }
    extension [A](curr: NamedDeltaBuffer[Obrem[A]]) def data: A = curr.state.data

  }

  def dotted[State](replicaID: LocalUid, init: State): NamedDeltaBuffer[Dotted[State]] =
    new NamedDeltaBuffer(replicaID, Dotted(init), List())

  def obrem[State: HasDots](replicaID: LocalUid, init: State): NamedDeltaBuffer[Obrem[State]] =
    new NamedDeltaBuffer(replicaID, Obrem(init), List())

  def dottedInit[State](replicaId: LocalUid, init: Dot => State): NamedDeltaBuffer[Dotted[State]] =
    val dot = Dots.empty.nextDot(replicaId.uid)
    NamedDeltaBuffer(replicaId, Dotted(init(dot), Dots.single(dot)), List())

}
