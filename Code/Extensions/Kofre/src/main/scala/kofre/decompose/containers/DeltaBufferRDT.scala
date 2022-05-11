package kofre.decompose.containers

import kofre.base.{Bottom, DecomposeLattice, Defs}
import kofre.causality.CausalContext
import kofre.contextual.{ContextDecompose, ContextLattice, WithContext}
import kofre.decompose.Delta
import kofre.syntax.{ArdtOpsContains, PermCausal, PermCausalMutate, PermIdMutate, PermQuery, WithNamedContext}

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
class DeltaBufferRDT[State](
    val state: WithContext[State],
    val replicaID: String,
    val deltaBuffer: List[WithNamedContext[State]]
) extends CRDTInterface[State, DeltaBufferRDT[State]] {

  def copy(
      state: WithContext[State] = state,
      deltaBuffer: List[WithNamedContext[State]] = deltaBuffer
  ): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](state, replicaID, deltaBuffer)

  override def applyDelta(delta: WithNamedContext[State])(implicit u: DecomposeLattice[WithContext[State]]): DeltaBufferRDT[State] =
    delta match {
      case WithNamedContext(origin, delta) =>
        DecomposeLattice[WithContext[State]].diff(state, delta) match {
          case Some(stateDiff) =>
            val stateMerged = DecomposeLattice[WithContext[State]].merge(state, stateDiff)
            copy(
              state = stateMerged,
              deltaBuffer = WithNamedContext(origin, stateDiff) :: deltaBuffer
            )
          case None => this.asInstanceOf[DeltaBufferRDT[State]]
        }
    }

  def resetDeltaBuffer(): DeltaBufferRDT[State] = copy(deltaBuffer = List())
}

object DeltaBufferRDT {

  given containsRelation[State]: ArdtOpsContains[DeltaBufferRDT[State], State] = new ArdtOpsContains[DeltaBufferRDT[State], State] {}


  given contextPermissions[L: ContextDecompose]: (PermIdMutate[DeltaBufferRDT[L], L] & PermCausalMutate[DeltaBufferRDT[L], L]) = CRDTInterface.dottedPermissions

  given fullPermission[L: DecomposeLattice]:  PermIdMutate[DeltaBufferRDT[L], L] = CRDTInterface.fullPermissions

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply[State](replicaID: String)(using bot: Bottom[WithContext[State]]): DeltaBufferRDT[State] =
    new DeltaBufferRDT[State](bot.empty, replicaID, List())

  def empty[State](replicaID: Defs.Id, init: State): DeltaBufferRDT[State] = new DeltaBufferRDT[State](WithContext(init), replicaID, List())
}
