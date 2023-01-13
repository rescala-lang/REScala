package kofre.deprecated.containers

import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.time.Dots
import kofre.dotted.{Dotted, DottedDecompose, DottedLattice}
import kofre.syntax.{Named, PermCausal, PermCausalMutate, PermIdMutate, PermQuery}

type DeltaBufferRDT[State] = DeltaBuffer[Dotted[State]]

object DeltaBufferRDT {

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply[State](replicaID: Id)(using bot: Bottom[Dotted[State]]): DeltaBuffer[Dotted[State]] =
    new DeltaBuffer(bot.empty, replicaID, List())

  def empty[State](replicaID: Id, init: State): DeltaBuffer[Dotted[State]] =
    new DeltaBuffer(Dotted(init), replicaID, List())

  def apply[State](replicaID: Id, init: State): DeltaBuffer[Dotted[State]] = empty(replicaID, init)
}

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class DeltaBuffer[State](
  state: State,
  replicaID: Id,
  deltaBuffer: List[State]
) {

  def applyDelta(delta: Named[State])(using DecomposeLattice[State]): DeltaBuffer[State] =
    applyDelta(delta.anon)

  def applyDelta(delta: State)(using DecomposeLattice[State]): DeltaBuffer[State] =
    DecomposeLattice[State].diff(state, delta) match {
      case Some(stateDiff) =>
        val stateMerged = DecomposeLattice[State].merge(state, stateDiff)
        new DeltaBuffer(stateMerged, replicaID, stateDiff :: deltaBuffer)
      case None => this
    }

  def resetDeltaBuffer(): DeltaBuffer[State] = copy(deltaBuffer = List())
}

object DeltaBuffer {

  given contextPermissions[L: DottedDecompose]
    : (PermIdMutate[DeltaBuffer[Dotted[L]], L] & PermCausalMutate[DeltaBuffer[Dotted[L]], L]) =
    new PermIdMutate[DeltaBuffer[Dotted[L]], L] with PermCausalMutate[DeltaBuffer[Dotted[L]], L] {
      override def replicaId(c: DeltaBuffer[Dotted[L]]): Id = c.replicaID
      override def mutate(c: DeltaBuffer[Dotted[L]], delta: L): DeltaBuffer[Dotted[L]] =
        c.applyDelta(Named(c.replicaID, Dotted(delta, Dots.empty)))
      override def query(c: DeltaBuffer[Dotted[L]]): L = c.state.store
      override def mutateContext(
        container: DeltaBuffer[Dotted[L]],
        withContext: Dotted[L]
      ): DeltaBuffer[Dotted[L]] = container.applyDelta(Named(container.replicaID, withContext))
      override def context(c: DeltaBuffer[Dotted[L]]): Dots = c.state.context
    }

  given fullPermission[L: DecomposeLattice: Bottom]: PermIdMutate[DeltaBuffer[Dotted[L]], L] =
    new PermIdMutate[DeltaBuffer[Dotted[L]], L] {
      override def replicaId(c: DeltaBuffer[Dotted[L]]): Id = c.replicaID
      override def mutate(c: DeltaBuffer[Dotted[L]], delta: L): DeltaBuffer[Dotted[L]] =
        c.applyDelta(Named(c.replicaID, Dotted(delta)))(using Dotted.latticeLift)
      override def query(c: DeltaBuffer[Dotted[L]]): L = c.state.store
    }
}
