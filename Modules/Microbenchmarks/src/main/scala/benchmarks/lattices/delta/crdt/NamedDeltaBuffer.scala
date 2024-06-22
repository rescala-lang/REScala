package benchmarks.lattices.delta.crdt

import rdts.base.{Lattice, Uid}
import rdts.dotted.{Dotted, DottedLattice}
import rdts.syntax.*
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

  inline def map(f: LocalUid ?=> State => State)(using Lattice[State]): NamedDeltaBuffer[State] =
    applyDelta(replicaID.uid, f(using replicaID)(state))

  def applyDelta(source: Uid, delta: State)(using Lattice[State]): NamedDeltaBuffer[State] =
    Lattice[State].diff(state, delta) match {
      case Some(stateDiff) =>
        val stateMerged = Lattice[State].merge(state, stateDiff)
        new NamedDeltaBuffer(replicaID, stateMerged, Named(source, stateDiff) :: deltaBuffer)
      case None => this
    }

  def clearDeltas(): NamedDeltaBuffer[State] = copy(deltaBuffer = List())

  def transform(f: State => State)(using Lattice[State]) = applyDelta(replicaID.uid, f(state))
}

object NamedDeltaBuffer {

  extension [A](curr: DeltaBufferDotted[A])(using Lattice[Dotted[A]])
    inline def mod(f: Dots ?=> A => Dotted[A]): DeltaBufferDotted[A] = {
      curr.applyDelta(curr.replicaID.uid, curr.state.mod(f(_)))
    }
  extension [A](curr: DeltaBufferDotted[A]) def data: A = curr.state.data

  implicit object workaround {
    extension [A](curr: NamedDeltaBuffer[A])(using Lattice[A])
      inline def mod(f: A => A): NamedDeltaBuffer[A] = {
        curr.applyDelta(curr.replicaID.uid, f(curr.state))
      }
  }

  def dotted[State](replicaID: Uid, init: State): NamedDeltaBuffer[Dotted[State]] =
    new NamedDeltaBuffer(replicaID, Dotted(init), List())

  def dottedInit[State](replicaId: Uid, init: Dot => State): NamedDeltaBuffer[Dotted[State]] =
    val dot = Dots.empty.nextDot(replicaId)
    NamedDeltaBuffer(replicaId, Dotted(init(dot), Dots.single(dot)), List())

  given dottedPermissions[L: DottedLattice]: PermCausalMutate[NamedDeltaBuffer[Dotted[L]], L] = new {
    override def query(c: NamedDeltaBuffer[Dotted[L]]): L = c.state.data
    override def mutateContext(
        container: NamedDeltaBuffer[Dotted[L]],
        withContext: Dotted[L]
    ): NamedDeltaBuffer[Dotted[L]] = container.applyDelta(container.replicaID.uid, withContext)
    override def context(c: NamedDeltaBuffer[Dotted[L]]): Dots = c.state.context
  }

  given plainPermissions[L: Lattice]: PermMutate[NamedDeltaBuffer[L], L] = new {
    override def mutate(c: NamedDeltaBuffer[L], delta: L): NamedDeltaBuffer[L] =
      c.applyDelta(c.replicaID.uid, delta)
    override def query(c: NamedDeltaBuffer[L]): L = c.state
  }
}
