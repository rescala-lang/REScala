package benchmarks.lattices.delta.crdt

import kofre.base.{Bottom, Uid, Lattice}
import kofre.dotted.{Dotted, DottedLattice}
import kofre.syntax.*
import kofre.time.Dots

type DeltaBufferDotted[State] = NamedDeltaBuffer[Dotted[State]]

case class Named[T](replicaId: Uid, anon: T)

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class NamedDeltaBuffer[State](
    replicaID: Uid,
    state: State,
    deltaBuffer: List[Named[State]] = Nil
) {

  def applyDelta(source: Uid, delta: State)(using Lattice[State]): NamedDeltaBuffer[State] =
    Lattice[State].diff(state, delta) match {
      case Some(stateDiff) =>
        val stateMerged = Lattice[State].merge(state, stateDiff)
        new NamedDeltaBuffer(replicaID, stateMerged, Named(source, stateDiff) :: deltaBuffer)
      case None => this
    }

  def clearDeltas(): NamedDeltaBuffer[State] = copy(deltaBuffer = List())
}

object NamedDeltaBuffer {

  def dotted[State](replicaID: Uid, init: State): NamedDeltaBuffer[Dotted[State]] =
    new NamedDeltaBuffer(replicaID, Dotted(init), List())

  given dottedPermissions[L: DottedLattice]: PermCausalMutate[NamedDeltaBuffer[Dotted[L]], L] = new {
    override def query(c: NamedDeltaBuffer[Dotted[L]]): L = c.state.store
    override def mutateContext(
        container: NamedDeltaBuffer[Dotted[L]],
        withContext: Dotted[L]
    ): NamedDeltaBuffer[Dotted[L]] = container.applyDelta(container.replicaID, withContext)
    override def context(c: NamedDeltaBuffer[Dotted[L]]): Dots = c.state.context
  }

  given plainPermissions[L: Lattice: Bottom]: PermMutate[NamedDeltaBuffer[L], L] = new {
    override def mutate(c: NamedDeltaBuffer[L], delta: L): NamedDeltaBuffer[L] =
      c.applyDelta(c.replicaID, delta)
    override def query(c: NamedDeltaBuffer[L]): L = c.state
  }
}
