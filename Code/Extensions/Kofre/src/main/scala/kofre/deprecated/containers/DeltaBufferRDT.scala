package kofre.deprecated.containers

import kofre.base.{Bottom, DecomposeLattice, Id}
import kofre.time.Dots
import kofre.dotted.{Dotted, DottedDecompose, DottedLattice}
import kofre.syntax.{Named, PermCausal, PermCausalMutate, PermIdMutate, PermQuery}

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class DeltaBufferRDT[State](
    state: Dotted[State],
    replicaID: Id,
    deltaBuffer: List[Named[Dotted[State]]]
) {

  def applyDelta(delta: Named[Dotted[State]])(using
      u: DecomposeLattice[Dotted[State]]
  ): DeltaBufferRDT[State] =
    delta match {
      case Named(origin, delta) =>
        DecomposeLattice[Dotted[State]].diff(state, delta) match {
          case Some(stateDiff) =>
            val stateMerged = DecomposeLattice[Dotted[State]].merge(state, stateDiff)
            new DeltaBufferRDT(stateMerged, replicaID, Named(origin, stateDiff) :: deltaBuffer)
          case None => this
        }
    }

  def resetDeltaBuffer(): DeltaBufferRDT[State] = copy(deltaBuffer = List())
}

object DeltaBufferRDT {

  given contextPermissions[L: DottedDecompose]
      : (PermIdMutate[DeltaBufferRDT[L], L] & PermCausalMutate[DeltaBufferRDT[L], L]) =
    new PermIdMutate[DeltaBufferRDT[L], L] with PermCausalMutate[DeltaBufferRDT[L], L] {
      override def replicaId(c: DeltaBufferRDT[L]): Id = c.replicaID
      override def mutate(c: DeltaBufferRDT[L], delta: L): DeltaBufferRDT[L] =
        c.applyDelta(Named(c.replicaID, Dotted(delta, Dots.empty)))
      override def query(c: DeltaBufferRDT[L]): L = c.state.store
      override def mutateContext(
          container: DeltaBufferRDT[L],
          withContext: Dotted[L]
      ): DeltaBufferRDT[L] = container.applyDelta(Named(container.replicaID, withContext))
      override def context(c: DeltaBufferRDT[L]): Dots = c.state.context
    }

  given fullPermission[L: DecomposeLattice: Bottom]: PermIdMutate[DeltaBufferRDT[L], L] =
    new PermIdMutate[DeltaBufferRDT[L], L] {
      override def replicaId(c: DeltaBufferRDT[L]): Id = c.replicaID
      override def mutate(c: DeltaBufferRDT[L], delta: L): DeltaBufferRDT[L] =
        c.applyDelta(Named(c.replicaID, Dotted(delta)))(using Dotted.latticeLift)
      override def query(c: DeltaBufferRDT[L]): L = c.state.store
    }

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply[State](replicaID: Id)(using bot: Bottom[Dotted[State]]): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](bot.empty, replicaID, List())

  def empty[State](replicaID: Id, init: State): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](Dotted(init), replicaID, List())

  def apply[State](replicaID: Id, init: State): DeltaBufferRDT[State] = empty(replicaID, init)
}
